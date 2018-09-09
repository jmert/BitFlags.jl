# BitFlags.jl

[![Build Status](https://travis-ci.org/jmert/BitFlags.jl.svg?branch=master)](https://travis-ci.org/jmert/BitFlags.jl)

`BitFlag.jl` provides an `Enum`-like type for bit flag option values. The main
motivations are:

1. Members have implicit numbering with incrementing powers of 2.
2. Binary OR (`|`) and AND (`&`) operations are supported among members.
3. Values are pretty-printed by name, with OR chains when multiple bits are
   set.

This implementation is a relatively minor modification of
[Julia](https://julialang.org/)'s `Enum` type implementation.

## Basic usage

To create a new `BitFlag` type, use the `@bitflag` macro, provide a name, an
optional integer type, and a list of the member options (and optional values).
A new definition can be given in inline form:
```julia
@bitflag BitFlagName[::BaseType] value1[=x] value2[=y]
```
or as a block definition:
```julia
@bitflag BitFlagName[::BaseType] begin
    value1[=x]
    value2[=y]
end
```

Automatic numbering starts at 1, but an initial flag value may be explicitly
set to the value of zero. If no explicit zero-valued member is given, then 0 is
not a valid value for the `BitFlag`. In the following example, we build an
8-bit `BitFlag` with no value for bit 3 (value of 4).
```julia
julia> @bitflag MyStyle::UInt8 S_NONE=0 S_BOLD S_ITALIC S_LARGE=8
```
Combinations can be made using standard binary operations:
```julia
julia> S_BOLD | S_LARGE
(S_BOLD | S_LARGE)::MyStyle = 0x09

julia> ans & S_ITALIC
S_NONE::MyStyle = 0x00
```
Conversion to and from integers is permitted, but only for valid combinations
of values:
```julia
julia> Int(S_BOLD)
1

julia> Integer(S_ITALIC)    # Abstract Integer uses native UInt8 type
0x02

julia> MyStyle(9)
(S_BOLD | S_LARGE)::MyStyle = 0x09

julia> MyStyle(4)    # MyStyle does not have a flag at 4
ERROR: ArgumentError: invalid value for BitFlag MyStyle: 4
Stacktrace:
...
```

## Printing

Each flag value is then printed with contextual information which is more
user-friendly than a raw integer:
```julia
julia> S_BOLD
S_BOLD::MyStyle = 0x00000001

julia> S_BOLD | S_LARGE
(S_BOLD | S_LARGE)::MyStyle = 0x00000005
```
In a compact context (such as in multi-dimensional arrays), the pretty-printing
takes on a shorter form:
```julia
julia> [S_NONE (S_BOLD | S_LARGE)]
1×2 Array{MyStyle,2}:
 S_NONE  S_BOLD|S_LARGE

julia> show(IOContext(stdout, :compact => true), S_BOLD | S_LARGE)
S_BOLD|S_LARGE
```

## Input/Output

`BitFlag`s support writing to and reading from streams as integers:
```julia
julia> io = IOBuffer();

julia> write(io, UInt8(9));

julia> seekstart(io);

julia> read(io, MyStyle)
(S_BOLD | S_LARGE)::MyStyle = 0x09
```
