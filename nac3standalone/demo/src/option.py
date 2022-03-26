@extern
def output_int32(x: int32):
    ...

class A:
    d: Option[int32]
    e: Option[Option[int32]]
    def __init__(self, a: Option[int32], b: Option[Option[int32]]):
        self.d = a
        self.e = b

def run() -> int32:
    a = Some(3)
    if a.is_some():
        d = a.unwrap()
        output_int32(a.unwrap())
        a = none
        if a.is_none():
            output_int32(d + 2)
    else:
        a = Some(5)
        c = Some(6)
        output_int32(a.unwrap() + c.unwrap())

    f = Some(4.3)
    output_int32(int32(f.unwrap()))

    obj = A(Some(6), none)
    output_int32(obj.d.unwrap())

    obj2 = Some(A(Some(7), none))
    output_int32(obj2.unwrap().d.unwrap())

    obj3 = Some(A(Some(8), Some(none)))
    if obj3.unwrap().e.unwrap().is_none():
        obj3.unwrap().e = Some(Some(9))
    output_int32(obj3.unwrap().d.unwrap())
    output_int32(obj3.unwrap().e.unwrap().unwrap())

    return 0
