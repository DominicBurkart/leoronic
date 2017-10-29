Welcome to the codebase for the LEORONIC LOW PERFORMANCE CLUSTER!

Current status: Development (not build passing).

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
    it'll deal with where to run what.

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
    non-linux/unix systems). Of course, if you want your cluster to perform
    tasks that require other languages or packages (e.g. Java, or Python with
    pandas), then you'll need to install them.

Installation:

  Assuming you're running linux/unix systems, you have bash ready to go.
  Next, you'll need to install erlang on all of your computers.

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

  Each node will now compile Leoronic's 'comrade.erl' to native code and launch
  its init function, which will instantiate the local worker queue and file
  management stores before trying to connect to every other node on the local
  area network. Note that Leoronic is not secure and should not be deployed in
  unsecured networks – it just uses Erlang's magic cookie system for "security."

  Next, we'll want to add commands to the cluster. If you're trying this out in
  the command line, you'll need to open a second one here to keep the erlang
  instance created by the first command running. First, source leoronic's
  utilities file:

    source leoronic_utilities

  Then, give your nodes something to do:

    leoronic_add_task 1024 1 1024 false "" "" "echo hello world"
    leoronic_add_task 1024 1 1024 false "" "" "echo hi there; echo folks"
    leoronic_add_task 1024 1 1024 false "" "" "echo what's up"

  The format is: requested available memory (bytes), cpu cores, storage (bytes,
  discluding the required storage space for any 'necessary files'), concurrency
  (true or false), required conditions (for dependencies or node specification),
  necessary files (files that should be copied to the working directory of the
  node so that it can perform its commands), and the bash commands that actually
  compose the task. Note that all listed conditions must be met for a command to
  be run.

  Example condition syntax:
    'queue:is_first;worker:has_file filename.txt'

  Condition syntax list:
    queue:
      is_first
        runs this task if all older tasks have been completed. Note that
        synchrony cross-nodes is not guaranteed, so if multiple nodes are adding
        new tasks, the exact ordering may vary. This is a unlikely to be an
        issue if all tasks are uploaded from a single source using the
        leoronic_add_task* commands in leoronic_utilities.
      all_with_cmd_done echo *
        runs this task if all tasks that match the regex 'echo *' are set as
        complete.
    worker:
      tag has_gpu
        runs this task only on a node with the 'has_gpu' tag. Tags are added by
        users via the leoronic_add_tag in leoronic_utilities. If a specific
        machine should always have specific tags, you can call leoronic_add_tag
        in the copy of the leoronic file on that node after you source
        leoronic_utilities.
      has_file filename.txt
        runs this task only on a node that has a given file in its leoronic
        directory (useful for avoiding resending large files over the network,
        as opposed to the 'necessary files' field, which renames said file to
        avoid cross-command conflicts).

Writing code for leoronic:

  Leoronic is pretty easy to write for, since it leaves dependencies and file
  i/o to you. You give it the files you want to source, either by delivering
  them to each node or linking them as necessary files, and you're ready to go.

  There are a few exceptions. For example, to avoid overwriting input files from
  other commands, leoronic will by default give each "necessary file" a new,
  unique name, and change all instances of the the previous name in the command
  to this unique name. This is not done on any files called in those commands,
  meaning that, for example, a command with two necessary files (a python script
  and a text file) will fail if the python script looks for the other necessary
  file in the local directory. This feature can be disabled to allow for this
  kind of programming, but the expected behavior is to simply pass filenames to
  linked program files as a parameter. Don't name your files the same thing as
  the commands you use – otherwise, those commands will also be replaced and
  your task won't run correctly.

  File output, on the other hand, is not modified. Thus, any files output by a
  task run on leoronic should have unique names, or else earlier-run tasks run
  on the same node will have their output data overwritten. This could be
  intentional – e.g., appending a relevant solution to file full of solutions.

  Best practice for leoronic is to keep all file i/o in the local directory or
  its subdirectories, and to instantiate leoronic while the working directory is
  a designated folder that you have read/write access to, since you can then use
  Leoronic's get_all_files function to receive the working directory (and its
  children) for each node. If this feature isn't important to you, then feel
  free to put your files wherever you'd like!

  After each task is run, the working directory is reset to whatever directory
  Leoronic was instantiated in. However, your scripts and commands can change
  the working directory as you see fit – and they'll stay changed until the end
  of the task.
