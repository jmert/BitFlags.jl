using BitFlags
using Test, Serialization

macro macrocall(ex)
    @assert Meta.isexpr(ex, :macrocall)
    ex.head = :call
    for i in 2:length(ex.args)
        ex.args[i] = QuoteNode(ex.args[i])
    end
    insert!(ex.args, 3, __module__)
    return esc(ex)
end

#@testset "Basic operation" begin
    # Inline definition
    @bitflag Flag1 flag1a flag1b flag1c
    @test Int(flag1a) == 1
    @test Int(flag1b) == 2
    @test Int(flag1c) == 4
    @test Flag1(1) == flag1a
    @test Flag1(2) == flag1b
    @test Flag1(4) == flag1c
    @test_throws ArgumentError Flag1(0)
    @test_throws ArgumentError Flag1(8)
    @test instances(Flag1) == (flag1a, flag1b, flag1c)

    # Block definition
    @bitflag Flag2 begin
        flag2a
        flag2b
        flag2c
    end
    @test Int(flag2a) == 1
    @test Int(flag2b) == 2
    @test Int(flag2c) == 4

    # Explicit numbering, inline
    @bitflag Flag3 flag3a=1 flag3b flag3c=8
    @test Int(flag3a) == 1
    @test Int(flag3b) == 2
    @test Int(flag3c) == 8
    @test typemin(Flag3) == flag3a
    @test typemax(Flag3) == flag3c
    @test Int(typemin(Flag3)) == 1
    @test Int(typemax(Flag3)) == 8
    @test_throws ArgumentError Flag3(4)

    # Zero-valued bit flag
    @bitflag Flag4 begin
        flag4a = 0
        flag4b
        flag4c
    end
    @test Int(flag4a) == 0
    @test Int(flag4b) == 1
    @test Int(flag4c) == 2
    @test Flag4(0) == flag4a

    # Mask operations
    @test Int(Flag1(7)) == 7
    @test Flag1(7) == flag1a | flag1b | flag1c
    @test Flag1(7) & flag1a == flag1a
    @test flag1a < flag1b < flag1c
    @test flag1a | flag1b < flag1c

    # Hashing
    @test Int(flag2a) == Int(flag3a)    # same numerical value, but
    @test hash(flag2a) != hash(flag3a)  # unique hashes as BitFlag
    @test which(hash, (Flag1, UInt)).sig != Tuple{typeof(hash), Flag1, UInt}
    struct NonstandardBitFlag <: BitFlags.BitFlag{UInt8} end
    let x = NonstandardBitFlag(), h = zero(UInt)
        @test hash(x, h) == invoke(hash, Tuple{Any, UInt}, x, h)
    end

    # Broadcasting
    @test [flag1a, flag1b] .| flag1c == [flag1a | flag1c, flag1b | flag1c]

    # ccall conversion
    @bitflag CFlag1::UInt64 begin
        cflag1_small = 1
        cflag1_large = UInt64(1) << 63
    end
    flag_nonzero(x::Integer) = Cint(!iszero(x))
    cflag_nonzero_u64 = @cfunction(flag_nonzero, Cint, (UInt64,))
    cflag_nonzero_u32 = @cfunction(flag_nonzero, Cint, (UInt32,))
    cflag_nonzero_i32 = @cfunction(flag_nonzero, Cint, (Int32,))
    @test ccall(cflag_nonzero_u64, Cint, (UInt64,), cflag1_small) == 1
    @test ccall(cflag_nonzero_u32, Cint, (UInt32,), cflag1_small) == 1
    @test ccall(cflag_nonzero_i32, Cint, (Int32,), cflag1_small) == 1
    @test ccall(cflag_nonzero_u64, Cint, (UInt64,), cflag1_large) == 1
    @test_throws InexactError ccall(cflag_nonzero_u32, Cint, (UInt32,), cflag1_large)
    @test_throws InexactError ccall(cflag_nonzero_i32, Cint, (Int32,), cflag1_large)
#end

#@testset "Documentation" begin
    # docstring literal
    """My Docstring""" @bitflag DocFlag1 docflag1a
    @test string(@doc(DocFlag1)) == "My Docstring\n"
    # docstring macro for non-string literals
    @doc raw"""Raw Docstring""" @bitflag DocFlag2 docflag2a
    @test string(@doc(DocFlag2)) == "Raw Docstring\n"
#end

#@testset "Type properties" begin
    # Default integer typing
    @bitflag Flag5 flag5a flag5b
    @test typeof(Integer(flag5a)) == UInt32
    @test typeof(Flag5) == DataType
    @test typeof(flag5a) <: Flag5 <: BitFlag
    @test isbitstype(Flag5)
    @test isbits(flag5a)

    # Construct non-default
    @bitflag Flag6::UInt8 flag6a flag6b flag6c
    @test typeof(Integer(flag6a)) == UInt8
    @test UInt8(flag6a) === 0x01
    @test UInt16(flag6b) === 0x0002
    @test UInt128(flag6c) === 0x00000000000000000000000000000004

    # Explicit values of non-default types
    @bitflag Flag7::UInt8 flag7a=big"1" flag7b=UInt8(2)
    @test Integer(flag7a) === UInt8(1)
    @test Integer(flag7b) === UInt8(2)
#end

#@testset "Internal definitions" begin
    # Underlying integer types
    @test BitFlags.basetype(Flag5) === UInt32
    @test BitFlags.basetype(Flag6) === UInt8

    # Whether flag has an explicit zero
    @test !BitFlags.haszero(Flag3)
    @test BitFlags.haszero(Flag4)

    # Key-Value mapping
    @test BitFlags.namemap(Flag5) isa NamedTuple{(:flag5a, :flag5b), NTuple{2, UInt32}}
    @test BitFlags.namemap(Flag6) isa NamedTuple{(:flag6a, :flag6b, :flag6c), NTuple{3, UInt8}}
    # Ensure the mapping can be used in a type-inferrable way
    function isset_nt(x::B) where {T, B <: BitFlag{T}}
        nm = BitFlags.namemap(B)
        K, V = keys(nm), values(nm)
        tf = (BitFlags.haszero(B) && iszero(T(x))) ? iszero.(V) : (!iszero).(V .& T.(x))
        return NamedTuple{K}(tf)
    end
    @test @inferred isset_nt(flag3a) == (; flag3a = true, flag3b = false, flag3c = false)
    @test @inferred isset_nt(flag3b) == (; flag3a = false, flag3b = true, flag3c = false)
    @test @inferred isset_nt(flag3b | flag3c) == (; flag3a = false, flag3b = true, flag3c = true)
    @test @inferred isset_nt(flag4a) == (; flag4a = true, flag4b = false, flag4c = false)
    @test @inferred isset_nt(flag4b) == (; flag4a = false, flag4b = true, flag4c = false)
    @test @inferred isset_nt(flag4b | flag4c) == (; flag4a = false, flag4b = true, flag4c = true)
#end

#@testset "Error conditions" begin

    @test_throws ArgumentError("no arguments given for BitFlag Foo"
                              ) @macrocall(@bitflag Foo)
    @test_throws ArgumentError("invalid argument for BitFlag Foo isa UInt32: bad expression head"
                              ) @macrocall(@bitflag Foo isa UInt32)
    @test_throws ArgumentError("invalid argument for BitFlag Foo: Foo::Float64; "
                               * "base type must be a bitstype unsigned integer"
                              ) @macrocall(@bitflag Foo::Float64 x=1.)

    # Require uniqueness
    @test_throws ArgumentError("invalid argument for BitFlag Foo: y = 1; value is not unique"
                              ) @macrocall(@bitflag Foo x=1 y=1)
    @test_throws ArgumentError("invalid argument for BitFlag Foo: y = 0; value is not unique"
                              ) @macrocall(@bitflag Foo x=0 y=0)
    @test_throws ArgumentError("invalid argument for BitFlag Foo: x; name is not unique"
                              ) @macrocall(@bitflag Foo x=0 x)

    # Explicit values must be powers of two
    @test_throws ArgumentError("invalid argument for BitFlag Foo: _three = 3; "
                               * "values must be a positive power of 2"
                              ) @macrocall(@bitflag Foo _three = 3)

    # Values must be integers
    @test_throws ArgumentError("invalid argument for BitFlag Foo: _zero = \"zero\"; "
                               * "values must be unsigned integers"
                              ) @macrocall(@bitflag Foo _zero="zero")
    @test_throws ArgumentError("invalid argument for BitFlag Foo: _zero = '0'; "
                               * "values must be unsigned integers"
                              ) @macrocall(@bitflag Foo _zero='0')
    @test_throws ArgumentError("invalid argument for BitFlag Foo: _zero = 0.5; "
                               * "values must be unsigned integers"
                              ) @macrocall(@bitflag Foo _zero=0.5)

    # Names must be valid identifiers
    @test_throws ArgumentError("""invalid argument for BitFlag Foo: if x
                                      1
                                  else
                                      2
                                  end"""
                              ) @macrocall(@bitflag Foo x ? 1 : 2)
    @test_throws ArgumentError("invalid argument for BitFlag Foo: 1 = 2"
                              ) @macrocall(@bitflag Foo 1=2)
    @test_throws ArgumentError("invalid argument for BitFlag Foo: #1; "
                               * "not a valid identifier"
                              ) @eval @macrocall(@bitflag Foo $(Symbol("#1")))

    # Disallow value overflow
    @test_throws ArgumentError("overflow in value \"y\" of BitFlag Foo"
                          ) @macrocall(@bitflag Foo x=(UInt32(1) << 31) y)

#end

#@testset "Input/Output" begin
    @bitflag Flag9::UInt64 flag9a=1 flag9b=256
    @bitflag Flag10::UInt8 flag10a=128 flag10b=2
    let io = IOBuffer()
        write(io, flag10a | flag10b)
        write(io, flag9b)
        write(io, flag10a)
        seekstart(io)
        @test read(io, Flag10) == flag10a | flag10b
        @test read(io, Flag9) == flag9b
        @test read(io, Flag10) == flag10a
    end
    let io = IOBuffer()
        serialize(io, flag9a)
        seekstart(io)
        @test deserialize(io) === flag9a
    end
#end

#@testset "String representations" begin
    @bitflag FilePerms::UInt8 NONE=0 READ=4 WRITE=2 EXEC=1
    # In submodule and not imported to test prefix printing
    module SubModule
        using ..BitFlags
        @bitflag Bits::UInt8 BIT_ONE BIT_TWO BIT_FOUR BIT_EIGHT # No-zero case
    end

    @test string(FilePerms) == "FilePerms"
    @test string(SubModule.Bits) == "Main.SubModule.Bits"
    @test string(NONE) == "NONE"
    @test string(SubModule.BIT_ONE) == "BIT_ONE"
    @test repr("text/plain", FilePerms) ==
        """BitFlag $(string(FilePerms)):
           NONE = 0x00
           EXEC = 0x01
           WRITE = 0x02
           READ = 0x04"""
    @test repr("text/plain", SubModule.Bits) ==
        """BitFlag Main.SubModule.Bits:
           BIT_ONE = 0x01
           BIT_TWO = 0x02
           BIT_FOUR = 0x04
           BIT_EIGHT = 0x08"""
    @test repr(EXEC) == "EXEC::FilePerms = 0x01"
    @test repr(SubModule.BIT_ONE) == "BIT_ONE::Main.SubModule.Bits = 0x01"
    @test repr(EXEC | READ) == "(READ | EXEC)::FilePerms = 0x05"
    @test repr(SubModule.BIT_ONE | SubModule.BIT_EIGHT) == "(BIT_EIGHT | BIT_ONE)::Main.SubModule.Bits = 0x09"
    @test repr(NONE | READ) == "READ::FilePerms = 0x04"

    let io = IOBuffer(),
        ioc = IOContext(io, :compact => true, :module => Main),
        iof = IOContext(io, :compact => false, :module => Main)
        # Explicit :compact => false required for consistency across Julia versions

        stringf(x) = (show(iof, MIME"text/plain"(), x); String(take!(io)))
        stringc(x) = (show(ioc, MIME"text/plain"(), x); String(take!(io)))

        @test stringf(BitFlag) == "BitFlag"
        @test stringc(BitFlag) == "BitFlag"

        @test stringf(Union{FilePerms, SubModule.Bits}) == "Union{FilePerms, Main.SubModule.Bits}"
        @test stringc(Union{FilePerms, SubModule.Bits}) == "Union{FilePerms, Bits}"

        @test stringc(NONE) == "NONE"
        @test stringc(SubModule.BIT_ONE) == "BIT_ONE"
        @test stringc(EXEC | READ) == "READ|EXEC"
        @test stringc(SubModule.BIT_ONE | SubModule.BIT_EIGHT) == "BIT_EIGHT|BIT_ONE"

        # Handle exceptional cases where set bits are not in the allowed set
        @test stringf(reinterpret(SubModule.Bits, 0x00)) == "reinterpret(Main.SubModule.Bits, 0x00)::Main.SubModule.Bits = 0x00"
        @test stringc(reinterpret(SubModule.Bits, 0x00)) == "0x00"

        @test stringf(reinterpret(SubModule.Bits, 0x11)) == "(BIT_ONE | reinterpret(Main.SubModule.Bits, 0x10))::Main.SubModule.Bits = 0x11"
        @test stringc(reinterpret(SubModule.Bits, 0x11)) == "BIT_ONE|0x10"

        @test stringf(reinterpret(FilePerms, 0x08)) == "reinterpret(FilePerms, 0x08)::FilePerms = 0x08"
        @test stringc(reinterpret(FilePerms, 0x08)) == "0x08"

        @test stringf(reinterpret(FilePerms, 0xff)) == "(READ | WRITE | EXEC | reinterpret(FilePerms, 0xf8))::FilePerms = 0xff"
        @test stringc(reinterpret(FilePerms, 0xff)) == "READ|WRITE|EXEC|0xf8"
    end
#end

#@testset "Scoped bit flags" begin
    # Individual feature tests are less stringent since most of the generated code is
    # the same as the extensively tested unscoped variety. Therefore, only do basic
    # functionality tests, and then test for properties specific to the scoped definition.

    # Inline definition
    @bitflagx SFlag1 flag1a flag1b flag1c
    @test SFlag1.T <: BitFlags.BitFlag
    @test Int(SFlag1.flag1a) == 1
    @test flag1a !== SFlag1.flag1a  # new value is scoped and distinct from unscoped name

    # Block definition
    @bitflagx SFlag2 begin
        flag2a
        flag2b
        flag2c
    end
    @test SFlag2.T <: BitFlags.BitFlag
    @test Int(SFlag2.flag2a) == 1
    @test flag2a !== SFlag2.flag2a  # new value is scoped and distinct from unscoped name

    # Inline definition with explicit type name
    @bitflagx T=U SFlag3 S=2 T
    @test SFlag3.U <: BitFlags.BitFlag
    @test SFlag3.T isa SFlag3.U
    @test Int(SFlag3.T) == 4

    # Block definition with explicit type name
    @bitflagx T=U SFlag4 begin
        S = 2
        T
    end
    @test SFlag4.U <: BitFlags.BitFlag
    @test SFlag4.T isa SFlag4.U
    @test Int(SFlag4.T) == 4

    # Definition with explicit integer type
    @bitflagx SFlag5::UInt8 flag1
    @test typeof(Integer(SFlag5.flag1)) === UInt8

    # Definition with both explicit integer type and type name
    @bitflagx T=_T SFlag6::UInt8 flag1
    @test SFlag6._T <: BitFlags.BitFlag
    @test typeof(Integer(SFlag6.flag1)) === UInt8

    # Documentation
    """My Docstring""" @bitflagx SDocFlag1 docflag
    @test string(@doc(SDocFlag1)) == "My Docstring\n"
    @doc raw"""Raw Docstring""" @bitflagx SDocFlag2 docflag
    @test string(@doc(SDocFlag2)) == "Raw Docstring\n"

    # Error conditions
    #   Too few arguments
    @test_throws ArgumentError("bad macro call: @bitflagx A = B"
                              ) @macrocall(@bitflagx A=B)
    #   Optional argument must be `T = $somesymbol`
    @test_throws ArgumentError("bad macro call: @bitflagx A = B Foo flag"
                              ) @macrocall(@bitflagx A=B Foo flag)
    @test_throws ArgumentError("bad macro call: @bitflagx T = 1 Foo flag"
                              ) @macrocall(@bitflagx T=1 Foo flag)

    # Printing
    @bitflagx SFilePerms::UInt8 NONE=0 READ=4 WRITE=2 EXEC=1
    module ScopedSubModule
        using ..BitFlags
        @bitflagx SBits::UInt8 BIT_ONE BIT_TWO BIT_FOUR BIT_EIGHT
    end

    @test string(SFilePerms.NONE) == "NONE"
    @test string(ScopedSubModule.SBits.BIT_ONE) == "BIT_ONE"
    @test repr("text/plain", SFilePerms.T) ==
        """BitFlag Main.SFilePerms.T:
           NONE = 0x00
           EXEC = 0x01
           WRITE = 0x02
           READ = 0x04"""
    @test repr("text/plain", ScopedSubModule.SBits.T) ==
        """BitFlag Main.ScopedSubModule.SBits.T:
           BIT_ONE = 0x01
           BIT_TWO = 0x02
           BIT_FOUR = 0x04
           BIT_EIGHT = 0x08"""
    @test repr(SFilePerms.EXEC) == "EXEC::SFilePerms.T = 0x01"
    @test repr(ScopedSubModule.SBits.BIT_ONE) == "BIT_ONE::Main.ScopedSubModule.SBits.T = 0x01"
#end
