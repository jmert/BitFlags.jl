# This is a light modification of Julia's @enum macro.
#
# Enums.jl is a part of Julia. License is MIT: https://julialang.org/license

module BitFlags

import Core.Intrinsics.bitcast
import Base.Meta.isexpr

export BitFlag, @bitflag

function namemap end
function haszero end

abstract type BitFlag{T<:Integer} end

basetype(::Type{<:BitFlag{T}}) where {T<:Integer} = T

(::Type{T})(x::BitFlag{T2}) where {T<:Integer,T2<:Unsigned} = T(bitcast(T2, x))::T
Base.cconvert(::Type{T}, x::BitFlag{T2}) where {T<:Unsigned,T2<:Unsigned} = T(x)
Base.write(io::IO, x::BitFlag{T}) where {T<:Unsigned} = write(io, T(x))
Base.read(io::IO, ::Type{T}) where {T<:BitFlag} = T(read(io, basetype(T)))

Base.isless(x::T, y::T) where {T<:BitFlag} = isless(basetype(T)(x), basetype(T)(y))
Base.:|(x::T, y::T) where {T<:BitFlag} = T(Integer(x) | Integer(y))
Base.:&(x::T, y::T) where {T<:BitFlag} = T(Integer(x) & Integer(y))

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
    for (i, sym) in Iterators.reverse(namemap(T))
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
        # explicitly setting :compact => false prints the type with its
        # "contextual path", i.e. MyFlag (for Main.MyFlag) or Main.SubModule.OtherFlags
        show(IOContext(io, :compact => false), typeof(x))
        print(io, " = ")
        show(io, Integer(x))
    end
end
function Base.show(io::IO, m::MIME"text/plain", t::Type{<:BitFlag})
    if isconcretetype(t)
        print(io, "BitFlag ")
        Base.show_datatype(io, t)
        print(io, ":")
        for (i, sym) in namemap(t)
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

@noinline function _throw_error(typename, s, msg = nothing)
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

julia> f(x::Items) = "I'm an Item with value: \$x"
f (generic function with 1 method)

julia> f(apple)
"I'm an Item with value: apple"

julia> f(apple | fork)
"I'm an Item with value: apple | fork"
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
    return _bitflag(__module__, T, Any[x...])
end

function _bitflag(__module__::Module, T::Union{Symbol, Expr}, x::Vector{Any})
    if T isa Symbol
        typename = T
        basetype = UInt32
    elseif isexpr(T, :(::), 2) && (e = T::Expr; e.args[1] isa Symbol)
        typename = e.args[1]::Symbol
        baseexpr = Core.eval(__module__, e.args[2])
        if !(baseexpr isa DataType) || !(baseexpr <: Unsigned) || !isbitstype(baseexpr)
            _throw_error(typename, T, "base type must be a bitstype unsigned integer")
        end
        basetype = baseexpr::Type{<:Unsigned}
    else
        _throw_error(typename, T)
    end
    if isempty(x)
        throw(ArgumentError("no arguments given for BitFlag $typename"))
    elseif length(x) == 1 && isexpr(x[1], :block)
        syms = (x[1]::Expr).args
    else
        syms = x
    end
    return _bitflag_impl(__module__, typename, basetype, syms)
end

function _bitflag_impl(__module__::Module, typename::Symbol, basetype::Type{<:Unsigned}, syms::Vector{Any})
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
                _throw_error(typename, s, "values must be unsigned integers")
            end
            i = convert(basetype, ei)::basetype
            if !iszero(i) && !ispow2(i)
                _throw_error(typename, s, "values must be a positive power of 2")
            end
        else
            _throw_error(typename, s)
        end
        if !Base.isidentifier(sym)
            _throw_error(typename, s, "not a valid identifier")
        end
        if (iszero(i) && maskzero) || (i & maskother) != 0
            _throw_error(typename, s, "value is not unique")
        end
        if sym in seen
            _throw_error(typename, s, "name is not unique")
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
    ebasetype = esc(basetype)

    n = length(names)
    namemap = Vector{Tuple{basetype, Symbol}}(undef, n)
    instances = Vector{Expr}(undef, n)
    flagconsts = Vector{Expr}(undef, n)
    @inbounds for ii in 1:length(names)
        sym, val = names[ii], values[ii]
        namemap[ii] = (val, sym)
        instances[ii] = :(bitcast($etypename, $val))
        flagconsts[ii] = :(const $(esc(sym)) = bitcast($etypename, $val))
    end

    blk = quote
        # bitflag definition
        Base.@__doc__(primitive type $etypename <: BitFlag{$ebasetype} $(8sizeof(basetype)) end)
        function $etypename(x::Integer)
            z = convert($ebasetype, x)
            $membershiptest || _argument_error($(Expr(:quote, typename)), x)
            return bitcast($etypename, z)
        end
        BitFlags.namemap(::Type{$etypename}) = $(esc(namemap))
        BitFlags.haszero(::Type{$etypename}) = $maskzero
        Base.typemin(x::Type{$etypename}) = $etypename($lo)
        Base.typemax(x::Type{$etypename}) = $etypename($hi)
        let flag_hash = hash($etypename)
            Base.hash(x::$etypename, h::UInt) = hash(flag_hash, hash(Integer(x), h))
        end
        Base.instances(::Type{$etypename}) = ($(instances...),)
        $(flagconsts...)
        nothing
    end
    blk.head = :toplevel
    return blk
end

end # module
