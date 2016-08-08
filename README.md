# leoronic
code for the Leoronic Low Performance Cluster 

## basic network structure
Leoronic is currently comprised of one head which manages the commands performed by an undefined number of workers. Leoronic is designed to adapt to the loss of workers while the cluster is running without performance instability or failure. Note that Leoronic is designed to be relatively slow, but highly robust to failure: commands to be run are saved to the head's storage and only removed when the command has been completed, so tasks that are interrupted by various system failures– including unexpected shutdown of the head– can usually be resumed without loss. Slow and steady wins the race!

## explanation of files
The head contains all of the information used by the cluster, which are the files in this repository.
- The "head" program runs on the head and calls other programs (eg "queues.py") to manage the functioning of the cluster. This is the central program of the Leoronic Low Performance Cluster.
- The subfolder "worker programs" is copied by the head to each worker so that each worker can run the commands sent to it by the head. Inside of it is the main program that workers run, "worker," which will match commands to the programs that perform them.
- Additionally, each machine has a "machine_information.txt" file, which will be used later to add more features to the cluster– think runtime estimators, intelligent assignment of intensive tasks, etc.
- Queues for each worker are created and managed by the head, and are rebalanced as necessary to distribute the cluster's list of tasks across all of its workers. The input currently comes from "startCommands.txt," and the queue files generated when the head runs look like this: "0queue.txt, 1queue.txt," etc. There are as many queue files as there are workers.

## creators
Dominic Burkart + Leor Freedman!
