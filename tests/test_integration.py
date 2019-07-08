from ..wrappers import *


def test_apply():
    assert apply(lambda x: x * x, 5) == 25


def test_apply_async():
    async_call = apply_async(lambda x: x * x, 5)
    r = async_call.get_record()
    assert handle_completed(r) == 25
    assert r.stdout == ""
    assert r.stderr == ""


def test_apply_async_print():
    async_print = apply_async(lambda x: print(x), "value of x printed to stdout")
    r = async_print.get_record()
    assert handle_completed(r) == None
    assert r.stderr == ""
    assert r.stdout == "value of x printed to stdout"


def test_blocking_functions():
    import multiprocessing

    def f(x: int, y: int) -> int:
        return x * y

    inputs = [(x, y) for x in range(5) for y in range(5)]
    starmap_output = starmap(f, inputs)
    map_output = map(f, inputs)
    imap_output = imap(f, inputs)
    imap_unordered_output = imap_unordered(f, inputs)

    with multiprocessing.Pool() as p:
        multi_output = p.starmap(f, inputs)

    assert starmap_output == multi_output
    assert map_output == multi_output
    assert imap_output == multi_output
    assert all(v in multi_output for v in imap_unordered_output)


def test_async_functions():
    import multiprocessing

    def f(x: int, y: int):
        return x * y

    inputs = [(x, y) for x in range(5) for y in range(5)]
    starmap_output = [a.get() for a in starmap_async(f, inputs)]
    map_output = [a.get() for a in map_async(f, inputs)]
    with multiprocessing.Pool() as p:
        multi_output = p.starmap(f, inputs)

    assert starmap_output == multi_output
    assert map_output == multi_output
