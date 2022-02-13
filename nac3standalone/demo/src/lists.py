@extern
def output_int32_list(x: list[int32]):
    ...

@extern
def output_int32(x: int32):
    ...

class A:
    a: int32
    b: bool
    def __init__(self, a: int32, b: bool):
        self.a = a
        self.b = b

def run() -> int32:
    data = [0, 1, 2, 3]

    output_int32_list(data[2:3])
    output_int32_list(data[:])
    output_int32_list(data[1:])
    output_int32_list(data[:-1])

    m1 = -1
    output_int32_list(data[::m1])
    output_int32_list(data[:0:m1])

    m2 = -2
    output_int32_list(data[m2::m1])

    # work around https://git.m-labs.hk/M-Labs/nac3/issues/188
    #get_list_slice()
    #list_slice_assignment()
    return 0

def get_list_slice():
    il = [i for i in range(15)]
    bl = [True, False, True, True, False, False, True, False, True, True]
    fl = [1.2, 2.3, 3.4, 4.5, 5.6, 6.7, 7.8, 8.9, 9.0, 10.1]
    al = [A(i, bl[i]) for i in range(len(bl))]
    tl = [(i, al[i], fl[i], (), (i, i + 1, bl[i])) for i in range(len(bl))]

    for l0 in [
        il[:],
        il[1:1],
        il[1:2],
        il[1:0],
        il[0:10:3],
        il[0::3],
        il[:-3:3],
        il[2:-3:3],
        il[:5:-1],
        il[-4:5:-1],
        il[-4:5:-2],
        il[-4:5:-3],
        il[-4::-3],
        il[::-3],
        il[::-1],
        il[3:5:-1],
        il[3:50:3],
        il[-20:-15:2],
        il[-20:-16:2],
        il[20:-13:-5],
        il[16:50:-1],
        il[:-13:-2],
        il[15:50:1],
    ]:
        output_int32_list(l0)
    
    for l1 in [
        bl[:],
        bl[1:1],
        bl[1:2],
        bl[1:0],
        bl[0:10:3],
        bl[0::3],
        bl[:-3:3],
        bl[2:-3:3],
        bl[:5:-1],
        bl[-4:5:-1],
        bl[-4:5:-2],
        bl[-4:5:-3],
        bl[-4::-3],
        bl[::-3],
        bl[::-1],
        bl[3:5:-1],
        bl[3:50:3],
        bl[-20:-15:2],
        bl[-20:-16:2],
        bl[20:-13:-5],
        bl[16:50:-1],
        bl[:-13:-2],
        bl[15:50:1],
    ]:
        output_int32_list([int32(b) for b in l1])

    for l2 in [
        fl[:],
        fl[1:1],
        fl[1:2],
        fl[1:0],
        fl[0:10:3],
        fl[0::3],
        fl[:-3:3],
        fl[2:-3:3],
        fl[:5:-1],
        fl[-4:5:-1],
        fl[-4:5:-2],
        fl[-4:5:-3],
        fl[-4::-3],
        fl[::-3],
        fl[::-1],
        fl[3:5:-1],
        fl[3:50:3],
        fl[-20:-15:2],
        fl[-20:-16:2],
        fl[20:-13:-5],
        fl[16:50:-1],
        fl[:-13:-2],
        fl[15:50:1],
    ]:
        output_int32_list([int32(f) for f in l2])
    
    for l3 in [
        al[:],
        al[1:1],
        al[1:2],
        al[1:0],
        al[0:10:3],
        al[0::3],
        al[:-3:3],
        al[2:-3:3],
        al[:5:-1],
        al[-4:5:-1],
        al[-4:5:-2],
        al[-4:5:-3],
        al[-4::-3],
        al[::-3],
        al[::-1],
        al[3:5:-1],
        al[3:50:3],
        al[-20:-15:2],
        al[-20:-16:2],
        al[20:-13:-5],
        al[16:50:-1],
        al[:-13:-2],
        al[15:50:1],
    ]:
        output_int32_list([a.a for a in l3])
        output_int32_list([int32(a.b) for a in l3])
    
    for l4 in [
        tl[:],
        tl[1:1],
        tl[1:2],
        tl[1:0],
        tl[0:10:3],
        tl[0::3],
        tl[:-3:3],
        tl[2:-3:3],
        tl[:5:-1],
        tl[-4:5:-1],
        tl[-4:5:-2],
        tl[-4:5:-3],
        tl[-4::-3],
        tl[::-3],
        tl[::-1],
        tl[3:5:-1],
        tl[3:50:3],
        tl[-20:-15:2],
        tl[-20:-16:2],
        tl[20:-13:-5],
        tl[16:50:-1],
        tl[:-13:-2],
        tl[15:50:1],
    ]:
        output_int32_list([t[0] for t in l4])
        output_int32_list([t[1].a for t in l4])
        output_int32_list([int32(t[1].b) for t in l4])
        output_int32_list([int32(t[2]) for t in l4])
        output_int32_list([t[4][0] for t in l4])
        output_int32_list([t[4][1] for t in l4])
        output_int32_list([int32(t[4][2]) for t in l4])

def list_slice_assignment():
    il = [i for i in range(15)]
    bl = [True, False, True, True, False, False, True, False, True, True]
    fl = [1.2, 2.3, 3.4, 4.5, 5.6, 6.7, 7.8, 8.9, 9.0, 10.1]
    al = [A(i, bl[i]) for i in range(len(bl))]
    tl = [(i, al[i], fl[i], (), (i, i + 1, bl[i])) for i in range(len(bl))]
    
    il1 = il[:]
    il1[2:5] = [99,98,97]
    output_int32_list(il1)
    il2 = il[:]
    il2[2:10:3] = [99,98,97]
    output_int32_list(il2)
    il3 = il[:]
    il3[12:4:-3] = [99,98,97]
    output_int32_list(il3)
    il4 = il[:]
    il4[4::-2] = [91,93,95]
    output_int32_list(il4)
    il5 = il[:]
    il5[3:-5] = []
    output_int32_list(il5)
    il6 = il[:]
    il6[3:-5] = [99,98,97]
    output_int32_list(il6)
    il7 = il[:]
    il7[:-2] = [99]
    output_int32_list(il7)
    il8 = il[:]
    il8[4:] = [99]
    output_int32_list(il8)

    bl1 = bl[:]
    bl1[2:5] = [False, True, True]
    output_int32_list([int32(b) for b in bl1])
    bl2 = bl[:]
    bl2[2:10:3] = [False, True, True]
    output_int32_list([int32(b) for b in bl2])
    bl3 = bl[:]
    bl3[12:4:-3] = [False, True]
    output_int32_list([int32(b) for b in bl3])
    bl4 = bl[:]
    bl4[4::-2] = [False, True, False]
    output_int32_list([int32(b) for b in bl4])
    bl5 = bl[:]
    bl5[3:-5] = []
    output_int32_list([int32(b) for b in bl5])
    bl6 = bl[:]
    bl6[3:-5] = [True, False, False]
    output_int32_list([int32(b) for b in bl6])
    bl7 = bl[:]
    bl7[:-2] = [False]
    output_int32_list([int32(b) for b in bl7])
    bl8 = bl[:]
    bl8[4:] = [True]
    output_int32_list([int32(b) for b in bl8])

    tl_3 = [
        (99, A(99, False), 99.88, (), (99, 100, True)),
        (98, A(98, False), 98.77, (), (98, 99, True)),
        (97, A(97, False), 97.66, (), (97, 98, True)),
    ]
    tl_2 = [
        (88, A(88, False), 88.77, (), (88, 89, True)),
        (87, A(87, False), 87.66, (), (87, 88, True)),
    ]
    tl_1 = [(78, A(78, False), 78.77, (), (78, 79, True)),]
    tl1 = tl[:]
    tl[2:5] = tl_3
    output_int32_list([t[0] for t in tl])
    output_int32_list([t[1].a for t in tl1])
    output_int32_list([int32(t[1].b) for t in tl1])
    output_int32_list([int32(t[2]) for t in tl1])
    output_int32_list([t[4][0] for t in tl1])
    output_int32_list([t[4][1] for t in tl1])
    output_int32_list([int32(t[4][2]) for t in tl1])
    tl2 = tl[:]
    tl2[2:10:3] = tl_3
    output_int32_list([t[0] for t in tl2])
    output_int32_list([t[1].a for t in tl2])
    output_int32_list([int32(t[1].b) for t in tl2])
    output_int32_list([int32(t[2]) for t in tl2])
    output_int32_list([t[4][0] for t in tl2])
    output_int32_list([t[4][1] for t in tl2])
    output_int32_list([int32(t[4][2]) for t in tl2])
    tl3 = tl[:]
    tl3[12:4:-3] = tl_2
    output_int32_list([t[0] for t in tl3])
    output_int32_list([t[1].a for t in tl3])
    output_int32_list([int32(t[1].b) for t in tl3])
    output_int32_list([int32(t[2]) for t in tl3])
    output_int32_list([t[4][0] for t in tl3])
    output_int32_list([t[4][1] for t in tl3])
    output_int32_list([int32(t[4][2]) for t in tl3])
    tl4 = tl[:]
    tl4[4::-2] = tl_3
    output_int32_list([t[0] for t in tl4])
    output_int32_list([t[1].a for t in tl4])
    output_int32_list([int32(t[1].b) for t in tl4])
    output_int32_list([int32(t[2]) for t in tl4])
    output_int32_list([t[4][0] for t in tl4])
    output_int32_list([t[4][1] for t in tl4])
    output_int32_list([int32(t[4][2]) for t in tl4])
    tl5 = tl[:]
    tl5[3:-5] = []
    output_int32_list([t[0] for t in tl5])
    output_int32_list([t[1].a for t in tl5])
    output_int32_list([int32(t[1].b) for t in tl5])
    output_int32_list([int32(t[2]) for t in tl5])
    output_int32_list([t[4][0] for t in tl5])
    output_int32_list([t[4][1] for t in tl5])
    output_int32_list([int32(t[4][2]) for t in tl5])
    tl6 = tl[:]
    output_int32(len(tl6))
    tl6[3:-5] = tl_2
    output_int32_list([t[0] for t in tl6])
    output_int32_list([t[1].a for t in tl6])
    output_int32_list([int32(t[1].b) for t in tl6])
    output_int32_list([int32(t[2]) for t in tl6])
    output_int32_list([t[4][0] for t in tl6])
    output_int32_list([t[4][1] for t in tl6])
    output_int32_list([int32(t[4][2]) for t in tl6])
    tl7 = tl[:]
    tl7[:-2] = tl_1
    output_int32_list([t[0] for t in tl7])
    output_int32_list([t[1].a for t in tl7])
    output_int32_list([int32(t[1].b) for t in tl7])
    output_int32_list([int32(t[2]) for t in tl7])
    output_int32_list([t[4][0] for t in tl7])
    output_int32_list([t[4][1] for t in tl7])
    output_int32_list([int32(t[4][2]) for t in tl7])
    tl8 = tl[:]
    tl8[4:] = tl_1
    output_int32_list([t[0] for t in tl8])
    output_int32_list([t[1].a for t in tl8])
    output_int32_list([int32(t[1].b) for t in tl8])
    output_int32_list([int32(t[2]) for t in tl8])
    output_int32_list([t[4][0] for t in tl8])
    output_int32_list([t[4][1] for t in tl8])
    output_int32_list([int32(t[4][2]) for t in tl8])