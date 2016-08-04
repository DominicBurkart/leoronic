import os
import sys

class distributer:
	jobs = [] #one-dimensional list of commands.
	
	queues = [] #two-dimensional list of commands.
	
	#when starting the cluster, jobs is populated by commmands
	#from a jobsFile passed into the begin method. Jobs is also
	#populated with the contents of queue files from the current
	#working directory when updating queues.

	def makeJobs(self, jobsFile):
		self.jobs = open(jobsFile).read().splitlines()

	def makeQueues(self, qNum):
		x = 0
		while (x < qNum):
			self.queues.append([])
			x += 1
	
	def distribute(self):
		cur = 0
		for job in self.jobs:
			self.queues[cur % len(self.queues)].append(job)
			cur += 1

	def saveQueues(self):
		cur = 0
		for queue in self.queues:
                        if str(cur)+"queue.txt" in os.listdir(os.getcwd()):
                                os.remove(str(cur)+"queue.txt")
			f = file(str(cur)+"queue.txt", mode='w')
			for command in queue:
				f.write(command + "\n")
			f.close()
			cur += 1

	def begin(self, qNum, jobsFile):
		self.makeJobs(jobsFile)
		self.makeQueues(qNum)
		self.distribute()
		self.saveQueues()

	def addQfile(self, filename):
                contents = open(filename).read().splitlines()
                if (contents != []):
                        self.jobs.extend(contents)
                        
	def update(self, qNum = -99): #i wrote this on a plane and couldn't look up how python does optional parameters.
                old_qNum = 0
                
		# reset datafields
		self.jobs = []
		self.queues = []

		# populate datafields with queue files in the current working directory
		filenames = os.listdir(os.getcwd())
		for filename in filenames:
                        if filename.endswith("queue.txt"):
                                self.addQfile(filename)
                                old_qNum += 1

                if qNum == -99:
                        qNum = old_qNum
                        
                
		# now balance the queues and we're done!
                self.makeQueues(qNum)
                self.distribute()
		self.saveQueues()

if sys.argv == ['distributer.py']:
        print "no parameters passed to distributer.py. Quitting program."
        sys.exit(1)

#required argument: whether the program is supposed to make the queue files or update them.
function = sys.argv[1] 
function = function.lower()

if (len(sys.argv) >= 3):
        qNum = int(sys.argv[2])

jobsFile = "startCommands.txt"
if (len(sys.argv) >= 4):
        jobsFile = arg[3]
        
d = distributer()
if (function == "begin" or function == "start"):
        d.begin(qNum, jobsFile)
elif (function == "update" or function == "balance" or function == "redistribute" or function == "distribute"):
        try:
                d.update(qNum)
        except NameError:
                d.update()
else:
        print "invalid input given: "+function
        print '''valid inputs:
        - begin, start
        - update, balance, redistribute, distribute
        '''


