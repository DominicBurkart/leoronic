from setuptools import setup, find_packages


def get_version():
    import subprocess
    import re

    line = subprocess.run(
        "grep vsn ebin/Leoronic.app ", shell=True, capture_output=True
    ).stdout.decode()
    return re.search('"(.*)"', line).group(1)


setup(
    name="leoronic",
    packages=find_packages(),
    version=get_version(),
    description="Simple distributed computing.",
    url="https://github.com/dominicburkart/leoronic",
    author="Dominic Burkart",
    author_email="dominicburkart@gmail.com",
)
