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

#@testset "Error conditions" begin

    @test_throws ArgumentError("no arguments given for BitFlag Foo"
                              ) @macrocall(@bitflag Foo)
    @test_throws ArgumentError("invalid base type for BitFlag Foo, "
                               * "::Float64; base type must be an unsigned "
                               * "integer primitive type"
                              ) @macrocall(@bitflag Foo::Float64 x=1.)

    # Require uniqueness
    @test_throws ArgumentError("values for BitFlag Foo are not unique"
                              ) @macrocall(@bitflag Foo x=1 y=1)
    @test_throws ArgumentError("values for BitFlag Foo are not unique"
                              ) @macrocall(@bitflag Foo x=0 y=0)

    # Explicit values must be powers of two
    @test_throws ArgumentError("invalid value for BitFlag Foo, _three = 3; "
                               * "values must be a positive power of 2"
                              ) @macrocall(@bitflag Foo _three = 3)

    # Values must be integers
    @test_throws ArgumentError("invalid value for BitFlag Foo, _zero = \"zero\"; "
                               * "values must be unsigned integers"
                              ) @macrocall(@bitflag Foo _zero="zero")
    @test_throws ArgumentError("invalid value for BitFlag Foo, _zero = '0'; "
                               * "values must be unsigned integers"
                              ) @macrocall(@bitflag Foo _zero='0')
    @test_throws ArgumentError("invalid value for BitFlag Foo, _zero = 0.5; "
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

    let io = IOBuffer(), ioc = IOContext(io, :compact => true, :module => Main)
        show(io, MIME"text/plain"(), BitFlag)
        @test String(take!(io)) == "BitFlag"
        show(ioc, MIME"text/plain"(), BitFlag)
        @test String(take!(io)) == "BitFlag"

        # Explicit :compact => false required for consistency across Julia versions
        iof = IOContext(io, :compact => false, :module => Main)
        show(iof, MIME"text/plain"(), Union{FilePerms, SubModule.Bits})
        @test String(take!(io)) == "Union{FilePerms, Main.SubModule.Bits}"
        show(ioc, MIME"text/plain"(), Union{FilePerms, SubModule.Bits})
        @test String(take!(io)) == "Union{FilePerms, Bits}"

        show(ioc, NONE)
        @test String(take!(io)) == "NONE"
        show(ioc, SubModule.BIT_ONE)
        @test String(take!(io)) == "BIT_ONE"
        show(ioc, EXEC | READ)
        @test String(take!(io)) == "READ|EXEC"
        show(ioc, SubModule.BIT_ONE | SubModule.BIT_EIGHT)
        @test String(take!(io)) == "BIT_EIGHT|BIT_ONE"
    end
#end

