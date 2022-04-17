from __future__ import annotations

@extern
def output_int32(x: int32):
    ...
@extern
def output_uint32(x: uint32):
    ...
@extern
def output_int64(x: int64):
    ...
@extern
def output_uint64(x: uint64):
    ...
@extern
def output_float64(x: float):
    ...

def run() -> int32:
    test_int32()
    test_uint32()
    test_int64()
    test_uint64()
    test_A()
    test_B()
    return 0

def test_int32():
    a = 17
    b = 3
    output_int32(a + b)
    output_int32(a - b)
    output_int32(a * b)
    output_int32(a // b)
    output_int32(a % b)
    output_int32(a | b)
    output_int32(a ^ b)
    output_int32(a & b)
    output_int32(a << b)
    output_int32(a >> b)
    output_float64(a / b)
    a += b
    output_int32(a)
    a -= b
    output_int32(a)
    a *= b
    output_int32(a)
    a //= b
    output_int32(a)
    a %= b
    output_int32(a)
    a |= b
    output_int32(a)
    a ^= b
    output_int32(a)
    a &= b
    output_int32(a)
    a <<= b
    output_int32(a)
    a >>= b
    output_int32(a)
    # fail because (a / b) is float
    # a /= b

def test_uint32():
    a = uint32(17)
    b = uint32(3)
    output_uint32(a + b)
    output_uint32(a - b)
    output_uint32(a * b)
    output_uint32(a // b)
    output_uint32(a % b)
    output_uint32(a | b)
    output_uint32(a ^ b)
    output_uint32(a & b)
    output_uint32(a << b)
    output_uint32(a >> b)
    output_float64(a / b)
    a += b
    output_uint32(a)
    a -= b
    output_uint32(a)
    a *= b
    output_uint32(a)
    a //= b
    output_uint32(a)
    a %= b
    output_uint32(a)
    a |= b
    output_uint32(a)
    a ^= b
    output_uint32(a)
    a &= b
    output_uint32(a)
    a <<= b
    output_uint32(a)
    a >>= b
    output_uint32(a)

def test_int64():
    a = int64(17)
    b = int64(3)
    output_int64(a + b)
    output_int64(a - b)
    output_int64(a * b)
    output_int64(a // b)
    output_int64(a % b)
    output_int64(a | b)
    output_int64(a ^ b)
    output_int64(a & b)
    output_int64(a << b)
    output_int64(a >> b)
    output_float64(a / b)
    a += b
    output_int64(a)
    a -= b
    output_int64(a)
    a *= b
    output_int64(a)
    a //= b
    output_int64(a)
    a %= b
    output_int64(a)
    a |= b
    output_int64(a)
    a ^= b
    output_int64(a)
    a &= b
    output_int64(a)
    a <<= b
    output_int64(a)
    a >>= b
    output_int64(a)

def test_uint64():
    a = uint64(17)
    b = uint64(3)
    output_uint64(a + b)
    output_uint64(a - b)
    output_uint64(a * b)
    output_uint64(a // b)
    output_uint64(a % b)
    output_uint64(a | b)
    output_uint64(a ^ b)
    output_uint64(a & b)
    output_uint64(a << b)
    output_uint64(a >> b)
    output_float64(a / b)
    a += b
    output_uint64(a)
    a -= b
    output_uint64(a)
    a *= b
    output_uint64(a)
    a //= b
    output_uint64(a)
    a %= b
    output_uint64(a)
    a |= b
    output_uint64(a)
    a ^= b
    output_uint64(a)
    a &= b
    output_uint64(a)
    a <<= b
    output_uint64(a)
    a >>= b
    output_uint64(a)

class A:
    a: int32
    def __init__(self, a: int32):
        self.a = a
    
    def __add__(self, other: A) -> A:
        output_int32(self.a + other.a)
        return A(self.a + other.a)

    def __sub__(self, other: A) -> A:
        output_int32(self.a - other.a)
        return A(self.a - other.a)

def test_A():
    a = A(17)
    b = A(3)
    
    c = a + b
    # fail due to alloca in __add__ function
    # output_int32(c.a)

    a += b
    # fail due to alloca in __add__ function
    # output_int32(a.a)
    
    a = A(17)
    b = A(3)
    d = a - b
    # fail due to alloca in __add__ function
    # output_int32(c.a)

    a -= b
    # fail due to alloca in __add__ function
    # output_int32(a.a)

    a = A(17)
    b = A(3)
    a.__add__(b)
    a.__sub__(b)


class B:
    a: int32
    def __init__(self, a: int32):
        self.a = a
    
    def __add__(self, other: B) -> B:
        output_int32(self.a + other.a)
        return B(self.a + other.a)

    def __sub__(self, other: B) -> B:
        output_int32(self.a - other.a)
        return B(self.a - other.a)
    
    def __iadd__(self, other: B) -> B:
        output_int32(self.a + other.a + 24)
        return B(self.a + other.a + 24)

    def __isub__(self, other: B) -> B:
        output_int32(self.a - other.a - 24)
        return B(self.a - other.a - 24)

def test_B():
    a = B(17)
    b = B(3)
    
    c = a + b
    # fail due to alloca in __add__ function
    # output_int32(c.a)

    a += b
    # fail due to alloca in __add__ function
    # output_int32(a.a)
    
    a = B(17)
    b = B(3)
    d = a - b
    # fail due to alloca in __add__ function
    # output_int32(c.a)

    a -= b
    # fail due to alloca in __add__ function
    # output_int32(a.a)

    a = B(17)
    b = B(3)
    a.__add__(b)
    a.__sub__(b)