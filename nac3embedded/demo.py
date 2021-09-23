from language import *


@kernel
class Demo:
    @kernel
    def run(self):
        pass


if __name__ == "__main__":
    Demo().run()
