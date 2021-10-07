from language import *
from artiq_builtins import *
from numpy import int32, int64

test_int = 123
test_float = 123.456
test_list = [1] * 1
test_list2 = [[1.1], [], [3.0]]
test_list_fail = [1, 2, 3.0]

test_tuple = (1, 2, 3.0)

@kernel
class Test:
    a: int32

    @kernel
    def __init__(self, a: int32):
        self.a = a

test = Test(1)
print(test.a)

@kernel
class Demo:
    @kernel
    def run(self):
        while True:
            delay_mu(round64(test_float * 2.0))
            delay_mu(int64(test_int))
            delay_mu(int64(test_list[0]))
            # delay_mu(int64(test_list_fail[0]))
            delay_mu(int64(test_tuple[0]))
            delay_mu(int64(test_tuple[2]))
            delay_mu(int64(test_list2[2][0]))

            delay_mu(int64(test.a))
            test.a = 10
            delay_mu(int64(test.a))


if __name__ == "__main__":
    run_on_core(Demo().run)
