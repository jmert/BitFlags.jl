# This is a light modification of Julia's @enum macro.
#
# Enums.jl is a part of Julia. License is MIT: https://julialang.org/license

module BitFlags

import Core.Intrinsics.bitcast
import Base.Meta.isexpr

export BitFlag, @bitflag, @bitflagx

function namemap end
function haszero end

abstract type BitFlag{T<:Integer} end

basetype(::Type{<:BitFlag{T}}) where {T<:Integer} = T

(::Type{T})(x::BitFlag{T2}) where {T<:Integer, T2} = T(bitcast(T2, x))::T
Base.cconvert(::Type{T}, x::BitFlag{T2}) where {T<:Integer, T2} = T(T2(x))
Base.write(io::IO, x::BitFlag{T}) where {T} = write(io, T(x))
Base.read(io::IO, ::Type{T}) where {T<:BitFlag} = T(read(io, basetype(T)))

Base.isless(x::T, y::T) where {T<:BitFlag} = isless(basetype(T)(x), basetype(T)(y))
Base.:|(x::T, y::T) where {T<:BitFlag} = T(Integer(x) | Integer(y))
Base.:&(x::T, y::T) where {T<:BitFlag} = T(Integer(x) & Integer(y))

Base.broadcastable(x::BitFlag) = Ref(x)

_bitflag_hash(x::BitFlag, h::UInt) = invoke(hash, Tuple{Any, UInt}, x, h)
Base.hash(x::BitFlag, h::UInt) = _bitflag_hash(x, h)

function Base.print(io::IO, x::T) where T<:BitFlag
    compact = get(io, :compact, false)::Bool
    xi = Integer(x)

    function _printnum(v)
        if compact
            show(io, v)
        else
            print(io, "reinterpret(")
            show(IOContext(io, :compact => false), T)
            print(io, ", ")
            show(io, v)
            print(io, ")")
        end
    end
    # abnormal case where bitflag is all unset but 0 not permitted
    if !haszero(T) && iszero(xi)
        _printnum(xi)
        return
    end
    multi = (haszero(T) ? !iszero(xi) : true) && !compact && !ispow2(xi)
    first = true
    sep = compact ? "|" : " | "
    for (sym, i) in Iterators.reverse(pairs(namemap(T)))
        if (first && iszero(i) && iszero(xi)) || !iszero(xi & i)
            if first
                multi && print(io, "(")
                first = false
            else
                print(io, sep)
            end
            print(io, sym)
            xi ⊻= i
        end
    end
    # abnormal case where set bits are not part of nominal set
    if !iszero(xi)
        !first && print(io, sep)
        _printnum(xi)
    end
    multi && print(io, ")")
    nothing
end
function Base.show(io::IO, x::BitFlag)
    if get(io, :compact, false)::Bool
        print(io, x)
    else
        print(io, x, "::")

        T = typeof(x)
        Tdef = parentmodule(T)
        from = get(io, :module, @static isdefined(Base, :active_module) ? Base.active_module() : Main)

        # Detect a scoped BitFlag inside a baremodule by looking for the implicit import
        # of Base bindings. For scoped bitflags, we actually care about whether the
        # module itself is visible instead of the type.
        isscoped = !isdefined(Tdef, :Base)
        sym = nameof(!isscoped ? T : Tdef)
        refmod = !isscoped ? Tdef : parentmodule(Tdef)
        if from === nothing || !Base.isvisible(sym, refmod, from)
            if !isscoped
                print(io, refmod, ".", sym)
            else
                print(io, Tdef, ".", nameof(T))
            end
        else
            if !isscoped
                print(io, sym)
            else
                print(io, nameof(Tdef), ".", nameof(T))
            end
        end
        print(io, " = ")
        show(io, Integer(x))
    end
end
function Base.show(io::IO, m::MIME"text/plain", t::Type{<:BitFlag})
    if isconcretetype(t)
        print(io, "BitFlag ")
        Base.show_datatype(io, t)
        print(io, ":")
        for (sym, i) in pairs(namemap(t))
            print(io, "\n", sym, " = ")
            show(io, Integer(i))
        end
    else
        invoke(show, Tuple{IO, typeof(m), Type}, io, m, t)
    end
end

@noinline function _argument_error(typename, x)
    throw(ArgumentError("invalid value for BitFlag $typename: $x"))
end

@noinline function _throw_macro_error(macroname, args)
    errmsg = "bad macro call: $(Expr(:macrocall, Symbol(macroname), nothing, args...))"
    throw(ArgumentError(errmsg))
end

@noinline function _throw_named_error(typename, s, msg = nothing)
    errmsg = "invalid argument for BitFlag $typename: $s"
    if msg !== nothing
        errmsg *= "; " * msg
    end
    throw(ArgumentError(errmsg))
end

"""
    @bitflag BitFlagName[::BaseType] value1[=x] value2[=y]

Create a `BitFlag{BaseType}` subtype with name `BitFlagName` and flag member values of
`value1` and `value2` with optional assigned values of `x` and `y`, respectively.
`BitFlagName` can be used just like other types and flag member values as regular values, such as

# Examples
```jldoctest itemflags
julia> @bitflag Items apple=1 fork=2 napkin=4

julia> f(x::Items) = "I'm a flag with value: \$x"
f (generic function with 1 method)

julia> f(apple)
"I'm a flag with value: apple"

julia> f(apple | fork)
"I'm a flag with value: (apple | fork)"
```

Values can also be specified inside a `begin` block, e.g.

```julia
@bitflag BitFlagName begin
    value1
    value2
end
```

`BaseType`, which defaults to [`UInt32`](@ref), must be a primitive subtype of `Unsigned`.
Member values can be converted between the bit flag type and `BaseType`. `read` and `write`
perform these conversions automatically. In case the bitflag is created with a non-default
`BaseType`, `Integer(value1)` will return the integer `value1` with the type `BaseType`.

To list all the instances of an bitflag use `instances`, e.g.

```jldoctest itemflags
julia> instances(Items)
(apple::Items = 0x00000001, fork::Items = 0x00000002, napkin::Items = 0x00000004)
```
"""
macro bitflag(T::Union{Symbol, Expr}, x::Union{Symbol, Expr}...)
    flagname, basetype = _parse_name(__module__, T)
    return _bitflag(__module__, nothing, flagname, basetype, Any[x...])
end

"""
    @bitflagx [T=FlagTypeName] BitFlagName[::BaseType] value1[=x] value2[=y]

Like [`@bitflag`](@ref) but instead scopes the new type `FlagTypeName` (named `T` if not
overridden via the first optional argument) and member constants within a module named
`BitFlagName`.

# Examples
```jldoctest scopedflags
julia> @bitflagx ScopedItems apple=1 fork=2 napkin=4

julia> f(x::ScopedItems.T) = "I'm a scoped flag with value: \$x"
f (generic function with 1 method

julia> f(ScopedItems.apple | ScopedItems.fork)
"I'm a scoped flag with value: (fork | apple)"
"""
macro bitflagx(arg1::Union{Symbol, Expr}, args::Union{Symbol, Expr}...)
    self = Symbol("@bitflagx")
    x = Any[args...]
    if isexpr(arg1, :(=), 2) && (e = arg1::Expr; (e.args[1] === :T && e.args[2] isa Symbol))
        # For this case, we need to decompose and swap symbols:
        # - `FlagTypeName` in `T = FlagTypeName` needs to get moved to the flagexpr argument
        # - `BitFlagName` in `BitFlagName[::BaseType]` becomes the scope name
        length(x) < 1 && _throw_macro_error(self, (arg1, args...))
        arg2 = popfirst!(x)
        flagname = arg1.args[2]
        scope, basetype = _parse_name(__module__, arg2)
        return _bitflag(__module__, scope, flagname, basetype, x)
    elseif isexpr(arg1, :(::), 2) && (e = arg1::Expr; e.args[1] isa Symbol)
        scope, basetype = _parse_name(__module__, arg1)
        return _bitflag(__module__, scope, :T, basetype, x)
    elseif arg1 isa Symbol
        return _bitflag(__module__, arg1, :T, UInt32, x)
    else
        _throw_macro_error(self, (arg1, args...))
    end
end

function _parse_name(__module__::Module, flagexpr::Union{Symbol, Expr})
    if flagexpr isa Symbol
        flagname = flagexpr
        basetype = UInt32
    elseif isexpr(flagexpr, :(::), 2) && (e = flagexpr::Expr; e.args[1] isa Symbol)
        flagname = e.args[1]::Symbol
        baseexpr = Core.eval(__module__, e.args[2])
        if !(baseexpr isa DataType) || !(baseexpr <: Unsigned) || !isbitstype(baseexpr)
            _throw_named_error(flagname, flagexpr, "base type must be a bitstype unsigned integer")
        end
        basetype = baseexpr::Type{<:Unsigned}
    else
        _throw_named_error(flagexpr, "bad expression head")
    end
    return (flagname, basetype)
end

function _bitflag(__module__::Module, scope::Union{Symbol, Nothing}, flagname::Symbol, basetype::Type{<:Unsigned}, x::Vector{Any})
    isempty(x) && throw(ArgumentError("no arguments given for BitFlag $flagname"))
    if length(x) == 1 && isexpr(x[1], :block)
        syms = (x[1]::Expr).args
    else
        syms = x
    end
    return _bitflag_impl(__module__, scope, flagname, basetype, syms)
end

function _bitflag_impl(__module__::Module, scope::Union{Symbol, Nothing}, typename::Symbol, basetype::Type{<:Unsigned},
                       syms::Vector{Any})
    names = Vector{Symbol}()
    values = Vector{basetype}()
    seen = Set{Symbol}()
    lo = hi = zero(basetype)
    maskzero, maskother = false, zero(basetype)
    i = oneunit(basetype)
    two = oneunit(basetype) + oneunit(basetype)

    for s in syms
        s isa LineNumberNode && continue
        if s isa Symbol
            if (i == typemin(basetype)) && (maskother & typemax(basetype) != 0)
                throw(ArgumentError("overflow in value \"$s\" of BitFlag $typename"))
            end
            sym = s::Symbol
        elseif isexpr(s, (:(=), :kw), 2) && (e = s::Expr; e.args[1] isa Symbol)
            sym = e.args[1]::Symbol
            ei = Core.eval(__module__, e.args[2]) # allow exprs, e.g. uint128"1"
            if !(ei isa Integer)
                _throw_named_error(typename, s, "values must be unsigned integers")
            end
            i = convert(basetype, ei)::basetype
            if !iszero(i) && !ispow2(i)
                _throw_named_error(typename, s, "values must be a positive power of 2")
            end
        else
            _throw_named_error(typename, s)
        end
        if !Base.isidentifier(sym)
            _throw_named_error(typename, s, "not a valid identifier")
        end
        if (iszero(i) && maskzero) || (i & maskother) != 0
            _throw_named_error(typename, s, "value is not unique")
        end
        if sym in seen
            _throw_named_error(typename, s, "name is not unique")
        end
        push!(seen, sym)
        push!(names, sym)
        push!(values, i)
        if iszero(i)
            maskzero = true
        else
            maskother |= i
        end
        if length(values) == 1
            lo = hi = i
        else
            lo = min(lo, i)
            hi = max(hi, i)
        end
        i = iszero(i) ? oneunit(i) : two*i
    end

    membershiptest = let zz = zero(basetype)
        maskzero ? :(z & $maskother !== $zz || z === $zz) : :(z & $maskother !== $zz)
    end

    order = sortperm(values)
    permute!(names, order)
    permute!(values, order)

    etypename = esc(typename)

    n = length(names)
    instances = Vector{Expr}(undef, n)
    flagconsts = Vector{Expr}(undef, n)
    @inbounds for ii in 1:length(names)
        sym, val = names[ii], values[ii]
        instances[ii] = :(bitcast($etypename, $val))
        flagconsts[ii] = :(const $(esc(sym)) = bitcast($etypename, $val))
    end
    namemap = NamedTuple{(names...,)}((values...,))

    blk = quote
        # bitflag definition
        primitive type $etypename <: BitFlag{$basetype} $(8sizeof(basetype)) end
        function $etypename(x::Integer)
            z = convert($basetype, x)
            $membershiptest || _argument_error($(Expr(:quote, typename)), x)
            return bitcast($etypename, z)
        end
        BitFlags.namemap(::Type{$etypename}) = $(esc(namemap))
        BitFlags.haszero(::Type{$etypename}) = $maskzero
        Base.typemin(x::Type{$etypename}) = $etypename($lo)
        Base.typemax(x::Type{$etypename}) = $etypename($hi)
        let flag_hash = hash($etypename)
            BitFlags._bitflag_hash(x::$etypename, h::UInt) = hash(flag_hash, hash(Integer(x), h))
        end
        Base.instances(::Type{$etypename}) = ($(instances...),)
        $(flagconsts...)
    end

    if scope isa Symbol
        escope = esc(scope)
        blk = quote
            baremodule $escope
                $(blk.args...)
            end
            Base.@__doc__ $escope
            nothing
        end
    else
        blk = quote
            $(blk.args...)
            Base.@__doc__ $etypename
            nothing
        end
    end
    blk.head = :toplevel

    return blk
end

end # module
