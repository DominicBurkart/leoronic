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
    Leoronic is not intended for public-facing clusters and currently does not
    support standard security procedures.

  - Reasonably Few Nodes.
    Leoronic is not designed to link together hundreds of computers. Every
    computer maintains an open socket with every other computer in the cluster,
    and chatter will increase (and eat up computational resources) for each
    node added.

  - Erlang and Bash Installed.
    While Leoronic was intended for use with late-2000s era machines using
    Debian or Ubuntu server builds, it can operate on any device with
    erlang and bash. Bash file headers may need to be tweaked for non-*nix
    systems.

To install erlang on your system:

  For Homebrew on OS X: brew install erlang
  For MacPorts on OS X: port install erlang
  For Ubuntu and Debian: apt-get install erlang
  For Fedora: yum install erlang
  For FreeBSD: pkg install erlang

(from https://www.erlang.org/downloads)

Required Bash commands (all standard in *nix):
