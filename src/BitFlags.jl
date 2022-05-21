# This is a light modification of Julia's @enum macro.
#
# Enums.jl is a part of Julia. License is MIT: https://julialang.org/license

module BitFlags

import Core.Intrinsics.bitcast
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
        for x in instances(t)
            print(io, "\n", Symbol(x), " = ")
            show(io, Integer(x))
        end
    else
        invoke(show, Tuple{IO, typeof(m), Type}, io, m, t)
    end
end

# generate code to test whether expr is in the given set of values
function membershiptest(expr, zmask)
    maskzero, maskother = zmask
    zz = zero(maskother)
    if maskzero == true
        :(($expr == $zz) || ($expr & $maskother != $zz))
    else
        :($expr & $maskother != $zz)
    end
end

@noinline bitflag_argument_error(typename, x) = throw(ArgumentError(string("invalid value for BitFlag $(typename): $x")))

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
macro bitflag(T::Union{Symbol,Expr}, syms...)
    if isempty(syms)
        throw(ArgumentError("no arguments given for BitFlag $T"))
    end
    basetype = UInt32
    typename = T
    if isa(T, Expr) && T.head === :(::) && length(T.args) == 2 && isa(T.args[1], Symbol)
        typename = T.args[1]
        basetype = Core.eval(__module__, T.args[2])
        if !isa(basetype, DataType) || !(basetype <: Unsigned) || !isbitstype(basetype)
            throw(ArgumentError("invalid base type for BitFlag $typename, ::$basetype; "
                                * "base type must be an unsigned integer primitive type"))
        end
    elseif !isa(T, Symbol)
        throw(ArgumentError("invalid type expression for bit flag $T"))
    end
    values = Vector{basetype}()
    seen = Set{Symbol}()
    namemap = Vector{Tuple{basetype,Symbol}}()
    lo = hi = zero(basetype)
    maskzero, maskother = false, zero(basetype)
    i = oneunit(basetype)
    two = oneunit(basetype) + oneunit(basetype)

    if length(syms) == 1 && syms[1] isa Expr && syms[1].head === :block
        syms = syms[1].args
    end
    for s in syms
        s isa LineNumberNode && continue
        if isa(s, Symbol)
            if (i == typemin(basetype)) && (maskother & typemax(basetype) != 0)
                throw(ArgumentError("overflow in value \"$s\" of BitFlag $typename"))
            end
        elseif isa(s, Expr) &&
               (s.head === :(=) || s.head === :kw) &&
               length(s.args) == 2 && isa(s.args[1], Symbol)
            i = Core.eval(__module__, s.args[2]) # allow exprs, e.g. uint128"1"
            if !isa(i, Integer)
                throw(ArgumentError("invalid value for BitFlag $typename, $s; " *
                                    "values must be unsigned integers"))
            end
            if !iszero(i) && !ispow2(i)
                throw(ArgumentError("invalid value for BitFlag $typename, $s; " *
                                    "values must be a positive power of 2"))
            end
            i = convert(basetype, i)
            s = s.args[1]
        else
            throw(ArgumentError(string("invalid argument for BitFlag ", typename, ": ", s)))
        end
        s = s::Symbol
        if !Base.isidentifier(s)
            throw(ArgumentError("invalid name for BitFlag $typename; "
                                * "\"$s\" is not a valid identifier"))
        end
        if (iszero(i) && maskzero) || (i & maskother) != 0
            throw(ArgumentError("values for BitFlag $typename are not unique"))
        end
        push!(namemap, (i,s))
        push!(values, i)
        if s in seen
            throw(ArgumentError("name \"$s\" in BitFlag $typename is not unique"))
        end
        push!(seen, s)
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
    order = sortperm([v[1] for v in namemap])
    permute!(namemap, order)
    permute!(values, order)
    blk = quote
        # bitflag definition
        Base.@__doc__(primitive type $(esc(typename)) <: BitFlag{$(basetype)} $(sizeof(basetype) * 8) end)
        function $(esc(typename))(x::Integer)
            $(membershiptest(:x, (maskzero,maskother))) ||
                bitflag_argument_error($(Expr(:quote, typename)), x)
            return bitcast($(esc(typename)), convert($(basetype), x))
        end
        BitFlags.namemap(::Type{$(esc(typename))}) = $(esc(namemap))
        BitFlags.haszero(::Type{$(esc(typename))}) = $(esc(maskzero))
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($lo)
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($hi)
        let flag_hash = hash($(esc(typename)))
            Base.hash(x::$(esc(typename)), h::UInt) = hash(flag_hash, hash(Integer(x), h))
        end
        let insts = (Any[$(esc(typename))(v) for v in $(values)]...,)
            Base.instances(::Type{$(esc(typename))}) = insts
        end
    end
    if isa(typename, Symbol)
        for (i, sym) in namemap
            push!(blk.args, :(const $(esc(sym)) = $(esc(typename))($i)))
        end
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module
