Welcome to the codebase for the LEORONIC LOW PERFORMANCE CLUSTER!

Leoronic is designed as a highly fault-tolerant cluster management system. If
you have some old computers and a router, you can turn them into a cluster using
Leoronic.

Features:

  - Efficiency.
    Leoronic was designed to be as lightweight as possible, giving you the
    performance of natively-compiled Erlang code and avoiding eating up
    precious system resources.

  - Flexibility.
    If your computers (or at least one of them) can run your code, Leoronic can
    coordinate it. Just give it a list of tasks and some runtime parameters and
    it'll deal with when to run what where.

  - Fault Tolerance.
    Leoronic's central features are tolerant to network failures. Even if
    every other computer disconnects from Leoronic, it will still go through
    the cluster's incomplete tasks until there are no remaining machines or all
    the tasks that the remaining machines are capable of doing are done.

Assumptions:

  - Network Security.
    Leoronic is not intended to be run on public-facing computers and currently
    does not support standard security procedures.

  - Reasonably Few Nodes.
    Leoronic is not designed to link together hundreds of computers. Every
    computer maintains an open socket with every other computer in the cluster,
    and chatter will increase (and eat up computational resources) for each
    node added.

  - Erlang and Bash Installed.
    While Leoronic was intended for use with late-2000s era machines using
    Debian or Ubuntu server builds, it can operate on any device with
    erlang and bash (though bash file headers may need to be tweaked for
    non-*nix systems). Of course, if you want your cluster to perform tasks that
    require other languages or packages (e.g. Java, or Python with pandas), then
    you'll need to install them.

Installation:

  Assuming you're running *nix systems, you have bash ready to go. Next, you'll
  need to install erlang on all of your computers.

  To install erlang on your system:

    For Homebrew on OS X: brew install erlang
    For MacPorts on OS X: port install erlang
    For Ubuntu and Debian: apt-get install erlang
    For Fedora: yum install erlang
    For FreeBSD: pkg install erlang

  (from https://www.erlang.org/downloads)

  Next, we can clone this repository locally:

    git clone https://github.com/DominicBurkart/leoronic.git

  Then, distribute it to each node. The location is irrelevant so long as you
  have some means of launching the bash program "leoronic."

Usage:

  On each node, navigate to wherever you placed the git repository and initiate
  the cluster:

    bash leoronic

  Next, we'll want to add commands to the cluster. First, source leoronic's
  utilities file:

    source leoronic_utilities

  Then, give your nodes something to do:

    leoronic_add_task 1024 1 1024 false [] [] ["echo hello world!"]
    leoronic_add_task 1024 1 1024 false [] [] ["echo hi!"]
    leoronic_add_task 1024 1 1024 false [] [] ["echo what's up???"]

  The format is: requested available memory (bytes), cpu cores, storage (bytes),
  concurrency (true or false), required tags for the worker, necessary files,
  and
