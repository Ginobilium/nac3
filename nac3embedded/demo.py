from language import *


class Demo:
    @kernel
    def run(self: bool) -> bool:
        return False


if __name__ == "__main__":
    Demo().run()
