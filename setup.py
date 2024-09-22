import setuptools.command.build
from setuptools import Command, setup

import os
import os.path
import subprocess

class MakeCommand(Command):
    """Class of `setuptools`/`distutils` commands which invoke a `make` program.

    GNU Make (http://www.gnu.org/software/make) is currently assumed for providing `make`. The program is invoked in a manner where it changes the working directory to the build directory advertised for the command (utilizing `self.set_undefined_options` as hinted at by [the documentation](http://setuptools.pypa.io/en/latest/userguide/extension.html) which defers to `help(setuptools.command.build.SubCommand)`).

    The command is expected to produce build artefacts which will be added to the wheel.
    """
    build_lib: str | None
    def finalize_options(self) -> None:
        self.set_undefined_options('build_py', ('build_lib', 'build_lib'))
    def initialize_options(self) -> None:
        self.build_lib = None
    def run(self, *args, **kwargs) -> None:
        os.makedirs(self.build_lib, exist_ok=True)
        subprocess.check_call(('make', '-C', self.build_lib, '-f', os.path.realpath('Makefile')))

class BuildCommand(setuptools.command.build.build):
    sub_commands = [ ('build_make', None) ] + setuptools.command.build.build.sub_commands # Makes the `build_make` command a sub-command of the `build_command`, which has the effect of the former being invoked when the latter is invoked (which is invoked in turn when the wheel must be built, through the `bdist_wheel` command)

setup(cmdclass={ 'build': BuildCommand, 'build_make': MakeCommand })
