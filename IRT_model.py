# -*- coding: utf-8 -*-
"""
Created on Wed 15:14 PM 2018

@author: tg422
"""
import random
import os
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import os
wd = "."
#wd = os.getcwd()
os.chdir(wd)


# Global variables
NUM_RUN, NUM_TEST = 10, 1 # no. runs in communications and teach simulations and no. tests
NUM_P, NUM_C, SAMPLE_FREQ = 50, np.int64(1e5), np.int64(2e3)  # no. agents for communications; no. communications; sample frequency
NUM_L, NUM_T, SAMPLE_FREQ_T = 200, np.int64(4e3), np.int64(5e1) # no. learners for teaching/testing; no. teaching/testing; sample frequency
NUM_M, NUM_U = 50, 50 # no. meanings and utterances
aff_num = 10 # how many other agents will be affected if the current communication is successful
epsilon = 0.001 # for calculating IC

# individual variables
upd_init = 'Hetero_both' # way of initializing update rate: 'Homo', all agents have the same update rate and frequency; 
                            # 'Hetero_updrate', agents can have different update rates
                            # 'Hetero_updfreq', agents can have different update frequencies
                            # 'Hetero_both', agents can have both different update rates and different update frequencies
# update rate
upd_way = 'Linear' # way of updating meaing-utterance mapping probability: 'Linear': linear update; 'Nonlinear': nonlinear update
updrate_mean, updrate_std = 0.3, 0.1 # mean and std of individual update rate; for 'Linear', mean=0.3; for 'Nonlinear', mean=0.9
updfreq_mean, updfreq_std = 0.5, 0.1 # mean and std of individual frequency of update M-U matrices
# noise in production and perception
addnoise = False # whether or not add noise in production and perception
noise_std = 1 # standard deviation of the Gaussian distribution of noise during production and perception



######
# Agent setting
######
class Agent(object):
    """
    Agent represents an individual:
        _ind -- index;
        _group -- string, "Uniform", "LowFreq", "HighFreq";
        _numM -- number of meaning;
        _MList -- name of known meaning;
        _numU -- number of utterance;
        _UList -- name of known utterance; 
        _prodDF -- production dataframe (matrix);
        _percDF -- perception dataframe (matrix);
        _updway -- way of update rate, 'Linear' or 'Nonlinear'
        _updrate -- rate of update (0.0 to 1.0)
        _updfreq -- frequency of update (0.0 to 1.0)
    """
    def __init__(self, ind, group, numM, MList, numU, UList, updway, updrate, updfreq):
        """
        initialization
        :param ind: index
        :param group: string, "Uniform", "LowFreq", "HighFreq";
        :param numM: number of meaning
        :param MList: list of meanings
        :param numU: number of utterance
        :param UList: list of utterance
        :param updway: way of update rate, 'Linear' or 'Nonlinear'
        :param updrate: rate of update (0.0 to 1.0)
        :param updfreq: frequency of update (0.0 to 1.0)
        """
        self.setInd(ind); self.setGroup(group); self.setnumM(numM); self.setMList(MList); self.setnumU(numU); self.setUList(UList);
        
        # initialize _prodDF
        matrix = np.zeros((self.getnumM(), self.getnumU()))
        self.setprodDF(pd.DataFrame(matrix, columns=self.getUList(), index=self.getMList()))
        # initialize each number, sum of each row is 1
        for m in self.getMList():
            self.getprodDF().loc[m,:] = np.random.dirichlet(np.ones(self.getnumU()),size=1)
        
        # initialize _percDF
        matrix = np.zeros((self.getnumM(), self.getnumU()))
        self.setpercDF(pd.DataFrame(matrix, columns=self.getUList(), index=self.getMList()))
        # initialize each number, sum of each column is 1
        for u in self.getUList():
            self.getpercDF().loc[:,u] = np.reshape(np.random.dirichlet(np.ones(self.getnumM()),size=1), (self.getnumM(), 1))
        
        # initialize updway, updrate and updfreq
        self.setupdway(updway); self.setupdrate(updrate); self.setupdfreq(updfreq)
    
    def getInd(self): return self._ind
    
    def setInd(self, ind):
        if ind < 0 or ind >= np.max([NUM_P, NUM_L]): raise ValueError("index out of range [0, " + str(np.max([NUM_P, NUM_L])) + ")")
        self._ind = ind
    
    def getGroup(self): return self._group
    
    def setGroup(self, group):
        if group != 'Uniform' and group != 'LowFreq' and group != 'HighFreq':
            raise ValueError("group input wrong, only 'Uniform', 'LowFreq', 'HighFreq' allowed!")
        self._group = group
        
    def getnumM(self): return self._numM
        
    def setnumM(self, numM): self._numM = numM
        
    def getnumU(self): return self._numU
    
    def setnumU(self, numU): self._numU = numU
    
    def getMList(self): return self._MList
    
    def setMList(self, MList): self._MList = MList
    
    def getUList(self): return self._UList
    
    def setUList(self, UList): self._UList= UList
    
    def getprodDF(self): return self._prodDF
    
    def setprodDF(self, prodDF): self._prodDF = prodDF
    
    def getpercDF(self): return self._percDF
    
    def setpercDF(self, percDF): self._percDF = percDF
    
    def getupdway(self): return self._updway
    
    def setupdway(self, updway):
        if updway != 'Linear' and updway != 'Nonlinear': raise ValueError("updway input wrong, only 'Linear' or 'Nonlinear' allowed!")
        self._updway = updway
    
    def getupdrate(self): return self._updrate
    
    def setupdrate(self, updrate):
        if updrate < 0.0 or updrate > 1.0: raise ValueError("updrate input wrong, only between 0.0 and 1.0 allowed!")
        self._updrate = updrate
    
    def getupdfreq(self): return self._updfreq
    
    def setupdfreq(self, updfreq):
        if updfreq < 0.0 or updfreq > 1.0: raise ValueError("updfreq input wrong, only between 0.0 and 1.0 allowed!")
        self._updfreq = updfreq        
    
    def __str__(self):
        return "Agent[" + str(self.getInd()) + "]: Group: " + self.getGroup() + "updway: " + self.getupdway() + "; updrate: " + str(self.getupdrate()) + "; updfreq: " + str(self.getupdfreq()) + "\nMList: " + str(self.getMList()) + "\nUList: " + str(self.getUList()) + "\nProdDF:\n" + str(self.getprodDF()) + "\nPercDF:\n" + str(self.getpercDF())
    
    # user defined functions
    def updVal(self, spli, sucfail, m, u):
        """
        update meaning-utterance mapping probability of a matrix
        :param spli: 'speaker' or 'listener'
        :param sucfail: 'success' or 'failed' of the commnication
        :param m: meaning index
        :param u: utterance index
        """
        if m in self.getMList() and u in self.getUList():
            if spli == 'speaker':
                if self.getupdway() == 'Linear':
                    if np.random.uniform() <= self.getupdfreq():
                        if sucfail == 'success': self.getprodDF().loc[m, u] += self.getupdrate()
                        if sucfail == 'failed': 
                            self.getprodDF().loc[m, u] -= self.getupdrate()
                            if self.getprodDF().loc[m, u] < epsilon: self.getprodDF().loc[m, u] = epsilon                    
                if self.getupdway() == 'Nonlinear':
                    if np.random.uniform() <= self.getupdfreq():
                        if sucfail == 'success': 
                            self.getprodDF().loc[m, u] = self.getprodDF().loc[m, u]*self.getupdrate() + (1-self.getupdrate())
                            for otheru in self.getUList():
                                if otheru != u: self.getprodDF().loc[m, otheru] = self.getprodDF().loc[m, otheru]*self.getupdrate()                    
                        if sucfail == 'failed': pass # for failed communication, speaker did nothing
                # clean too small value with 0
                for i in self.getUList():
                    if self.getprodDF().loc[m, i] == epsilon: self.getprodDF().loc[m, i] = 0.0
                # renormalization of the same row
                self.getprodDF().loc[m, :] /= np.sum(self.getprodDF().loc[m, :])                
                    
            if spli == 'listener':
                if self.getupdway() == 'Linear':
                    if np.random.uniform() <= self.getupdfreq():
                        if sucfail == 'success': self.getpercDF().loc[m, u] += self.getupdrate()
                        if sucfail == 'failed': 
                            self.getpercDF().loc[m, u] -= self.getupdrate()
                            if self.getpercDF().loc[m, u] < epsilon: self.getpercDF().loc[m, u] = epsilon                    
                if self.getupdway() == 'Nonlinear':
                    if np.random.uniform() <= self.getupdfreq():
                        if sucfail == 'success': 
                            self.getpercDF().loc[m, u] = self.getpercDF().loc[m, u]*self.getupdrate() + (1-self.getupdrate())
                            for otherm in self.getMList():
                                if otherm != m: self.getpercDF().loc[otherm, u] = self.getpercDF().loc[otherm, u]*self.getupdrate()                 
                        if sucfail == 'failed':
                            self.getpercDF().loc[m, u] = self.getpercDF().loc[m, u]*self.getupdrate()                
                # clean too small value with 0
                for i in self.getMList():
                    if self.getpercDF().loc[i, u] == epsilon: self.getpercDF().loc[i, u] = 0.0
                # renormalization of the same column
                self.getpercDF().loc[:, u] /= np.sum(self.getpercDF().loc[:, u])
                
        else:
            if m not in self.getMList():
                # add new meaning
                pass
            if u not in self.getUList():
                # add utterance
                pass
    
    def roulette(self, probList):
        """
        use roulette wheel to select an utterance or meaning
        :param probList: list of probabilities from a row of prodDF or a column of percDF
        :return index: index of the chosen row/column in probList
        """
        roulettewheel = probList.copy()
        for i in range(1,len(roulettewheel)):
            roulettewheel[i] = roulettewheel[i] + roulettewheel[i-1]
        index = -1
        r = np.random.uniform()
        for i in range(len(roulettewheel)):
            if r <= roulettewheel[i]:
                index = i; break
        assert(index!=-1)
        return index
    
    def addnoise(self, mu, MUList):
        """
        add noice to mu based on Gaussian distributed noises
        :param mu: meaning or utterance of the produced or perceived
        "param MUList: list of all meaning or utterance
        """
        if addnoise:
            ind = MUList.index(mu)
            assert(ind != -1)
            if len(mu) >= 5:
                if ind >= 2 and ind <= len(MUList)-3: candList = [MUList[ind-2], MUList[ind-1], MUList[ind], MUList[ind+1], MUList[ind+2]]
                if ind == 1: candList = [MUList[ind+2], MUList[ind-1], MUList[ind], MUList[ind+1], MUList[ind+2]]
                if ind == 0: candList = [MUList[ind+2], MUList[ind+1], MUList[ind], MUList[ind+1], MUList[ind+2]]
                if ind == len(MUList) - 2: candList = [MUList[ind-2], MUList[ind-1], MUList[ind], MUList[ind+1], MUList[ind-2]]
                if ind == len(MUList) - 1: candList = [MUList[ind-2], MUList[ind-1], MUList[ind], MUList[ind-1], MUList[ind-2]]
            elif len(mu) == 4:
                if ind >= 1 and ind <= len(MUList)-2: candList = [MUList[ind-1], MUList[ind], MUList[ind+1]]
                if ind == 0: candList = [MUList[ind+1], MUList[ind], MUList[ind+1]]
                if ind == 3: candList = [MUList[ind-1], MUList[ind], MUList[ind-1]]
            elif len(mu) == 3:
                if ind == 1: candList = [MUList[ind-1], MUList[ind], MUList[ind+1]]
                if ind == 0: candList = [MUList[ind+1], MUList[ind], MUList[ind+1]]
                if ind == 2: candList = [MUList[ind-1], MUList[ind], MUList[ind-1]]
            else: candList = [MUList[ind]]
            # select based on Gaussian distribution
            while True:
                index = int(random.normalvariate(ind, noise_std) + 0.5)
                if 0 <= index and index < len(candList): return candList[index]
        else: return mu            
            
    def speak(self, meaning, maxVal=False):
        """ 
        speak: produce an utterance to encode a meaning based on prodDF
        :param meaning: meaning to produce
        :param maxVal: whether use the maximum value in the matrix row/column (True) or not (False), default is False (use roulette wheel)
        :return utterance: utterance to encode that meaning
        """
        assert(meaning in self.getMList())
        # select utterance based on probability of prodDF.loc[meaning, :]
        probList = self.getprodDF().loc[meaning, :]
        if not maxVal:
            utterance = self.getprodDF().columns[self.roulette(probList)]
            utterance = self.addnoise(utterance, self.getUList())
        else: utterance = self.getprodDF().columns[probList==np.max(probList)][0]
        return utterance
        
    def listen(self, utterance, maxVal=False):
        """ 
        listen: perceive an utterance to a meaning based on percDF
        :param utterance: utterance to perceive
        :param maxVal: whether use the maximum value in the matrix row/column (True) or not (False), default is False (use roulette wheel)
        :return meaning: meaning that the listener believes that the utterance encodes
        """
        if utterance not in self.getUList(): return -1
        assert(utterance in self.getUList())
        # select utterance based on probability of prodDF.loc[meaning, :]
        probList = self.getpercDF().loc[:,utterance]
        if not maxVal:
            meaning = self.getpercDF().index[self.roulette(probList)]
            meaning = self.addnoise(meaning, self.getMList())
        else: meaning = self.getpercDF().index[probList==np.max(probList)][0]
        return meaning
        
      
######
# functions for running
######
def calSI(SItype, pop, ag1, ag2):
    """
    calculate similarity between vocabularies of two agents
    :param SItype: type of SI calculation: 0, considering both prod and perc; 1, considering only perc
    :param pop: list of population
    :param ag1: index of agent1
    :param ag2: index of agent2
    
    :return si: similarilty value
    """
    commM, commU = list(set(pop[ag1].getMList()).intersection(pop[ag2].getMList())), list(set(pop[ag1].getUList()).intersection(pop[ag2].getUList()))
    lenM, lenU = len(commM), len(commU)
    if SItype == 0:
        prod_ag1, prod_ag2 = pop[ag1].getprodDF().loc[commM, commU], pop[ag2].getprodDF().loc[commM, commU]
        prod_abs = np.absolute(prod_ag1 - prod_ag2); prod_abs_sum = sum(prod_abs.sum(axis=1))
        perc_ag1, perc_ag2 = pop[ag1].getpercDF().loc[commM, commU], pop[ag2].getpercDF().loc[commM, commU]
        perc_abs = np.absolute(perc_ag1 - perc_ag2); perc_abs_sum = sum(perc_abs.sum(axis=0))
        si = 1 - 1.0/np.float((lenU-1)*lenM + (lenM-1)*lenU)*(prod_abs_sum+perc_abs_sum)
    if SItype == 1:
        perc_ag1, perc_ag2 = pop[ag1].getpercDF().loc[commM, commU], pop[ag2].getpercDF().loc[commM, commU]
        perc_abs = np.absolute(perc_ag1 - perc_ag2); perc_abs_sum = sum(perc_abs.sum(axis=0))
        si = 1 - 1.0/np.float((lenU-1)*lenM + (lenM-1)*lenU)*(perc_abs_sum)
    return si


def calIC(ICtype, pop, ag):
    """
    calculate degree of consistency between an individual's prodDF and percDF
    :param ICtype: type of IC calculation: 0, considering both prod and perc; 1, considering only perc
    :param pop: list of population
    :param ag: index of agent
    
    :return IC: degree of consistency value
    """
    lenM, lenU = len(pop[ag].getMList()), len(pop[ag].getUList())
    if ICtype == 0:
        prod_copy = pop[ag].getprodDF().copy()
        prod_copy[prod_copy <= epsilon] = 1; prod_copy[(prod_copy > epsilon)&(prod_copy != 1)] = 0; sum_prod = sum(prod_copy.sum(axis=1))
        perc_copy = pop[ag].getpercDF().copy()
        perc_copy[perc_copy <= epsilon] = 1; perc_copy[(perc_copy > epsilon)&(perc_copy != 1)] = 0; sum_perc = sum(perc_copy.sum(axis=0))
        ic = 0.5*(1.0/np.float64(lenM*(lenU-1))*sum_prod + 1.0/np.float(lenU*(lenM-1))*sum_perc)    
    if ICtype == 1:
        perc_copy = pop[ag].getpercDF().copy()
        perc_copy[perc_copy <= epsilon] = 1; perc_copy[(perc_copy > epsilon)&(perc_copy != 1)] = 0; sum_perc = sum(perc_copy.sum(axis=0))
        ic = 1.0/np.float(lenU*(lenM-1))*sum_perc    
    return ic


def calPC(PCtype, pop, ag1, ag2):
    """
    calculate population convergence between two agents
    :param PCtype: type of PC calculation: 0, considering both prod and perc; 1, considering only perc
    :param pop: list of population
    :param ag1: index of agent1
    :param ag2: index of agent2
    
    :return pc: population convergence value
    """
    commM, commU = list(set(pop[ag1].getMList()).intersection(pop[ag2].getMList())), list(set(pop[ag1].getUList()).intersection(pop[ag2].getUList()))
    lenM, lenU = len(commM), len(commU)
    if PCtype == 0:
        prod_ag1, prod_ag2 = pop[ag1].getprodDF().loc[commM, commU], pop[ag2].getprodDF().loc[commM, commU]
        perc_ag1, perc_ag2 = pop[ag1].getpercDF().loc[commM, commU], pop[ag2].getpercDF().loc[commM, commU]
        multi1 = np.multiply(prod_ag1, perc_ag2); sum1 = sum(multi1.sum(axis=1))
        multi2 = np.multiply(perc_ag1, prod_ag2); sum2 = sum(multi2.sum(axis=0))
        pc = 1.0/(2*np.float64(max(lenM, lenU)))*(sum1+sum2)
    if PCtype == 1:
        perc_ag1, perc_ag2 = pop[ag1].getpercDF().loc[commM, commU], pop[ag2].getpercDF().loc[commM, commU]
        multi1 = np.multiply(perc_ag1, perc_ag2); sum1 = sum(multi1.sum(axis=1)); sum2 = sum(multi1.sum(axis=0))
        pc = 1.0/(2*np.float64(max(lenM, lenU)))*(sum1+sum2)
    return pc


def calCS(CStype, pop, ag1, ag2):
    """
    calculate communicative success between two agents
    :param CStype: type of CS calculation: 0, considering both prod and perc; 1, considering only perc
    :param pop: list of population
    :param ag1: index of agent1
    :param ag2: index of agent2
    
    :return cs: communicative success
    """
    cs = 0.0
    if CStype == 0:
        # com 1: ag1 -> ag2; com 2: ag2 -> ag1
        for i in range(NUM_M):
            meaning = 'M' + str(i)
            utterance = pop[ag1].speak(meaning) # production
            perceive = pop[ag2].listen(utterance) # perception
            if meaning == perceive: cs += 1.0
            utterance = pop[ag2].speak(meaning) # production
            perceive = pop[ag1].listen(utterance) # perception
            if meaning == perceive: cs += 1.0
        cs /= 2.0    
    
    if CStype == 1:
        # com for all utterance
        for i in range(NUM_U):
            utterance = 'U' + str(i)
            perceive1 = pop[ag1].listen(utterance) # perception
            perceive2 = pop[ag2].listen(utterance) # perception
            if perceive1 == perceive2: cs += 1.0            
    
    return cs
    

def recRes(caltype, com, pop, file1, file2, file3, file4):
    """
    calculate measures and record results
    :param caltype: type of SI, IC, PC, and CS calculation: 0, considering both prod and perc; 1, considering only perc
    :param com: number of communications (com+1)
    :param nopop: population size
    :param file1: name of result file (store measures)
    :param file2: name of result file (store individual matrix)
    :param file3: name of result file (store pairwised CS)
    :param file4: name of result file (store offset from group CS)
    """
    # calculate some measures 
    SI, IC, PC, CS = 0.0, 0.0, 0.0, 0.0; numSI_PC, numIC = 0, 0    
    # pair-wised communicative success
    CS_pair = pd.DataFrame(np.zeros([len(pop), len(pop)]))
    
    if caltype == 0: SItype, ICtype, PCtype, CStype = 0, 0, 0, 0
    if caltype == 1: SItype, ICtype, PCtype, CStype = 1, 1, 1, 1
    
    for ag1 in range(len(pop)):
        IC += calIC(ICtype, pop, ag1); numIC += 1    
        for ag2 in range(ag1+1, len(pop)):
            SI += calSI(SItype, pop, ag1, ag2)
            PC += calPC(PCtype, pop, ag1, ag2)
            pairCS = calCS(CStype, pop, ag1, ag2)
            CS += pairCS
            if CStype == 0: CS_pair.loc[ag1, ag2] = pairCS/np.float64(NUM_M)
            if CStype == 1: CS_pair.loc[ag1, ag2] = pairCS/np.float64(NUM_U)
            numSI_PC += 1
    SI, IC, PC, CS = SI/np.float64(numSI_PC), IC/np.float64(numIC), PC/np.float64(numSI_PC), CS/(np.float64(numSI_PC)*np.float64(NUM_M))
    
    # pair-wised offset from the group mean CS
    CS_offset = pd.DataFrame(np.zeros([len(pop), len(pop)]))    
    for ag1 in range(len(pop)):
        for ag2 in range(ag1+1, len(pop)):
            CS_offset.loc[ag1, ag2] = CS_pair.loc[ag1, ag2] - CS
    
    print "com={}; SI={:6.4f}; IC={:6.4f}; PC={:6.4f}; CS={:6.4f}".format(com, SI, IC, PC, CS)
    # store results
    f1 = open(file1, 'a+')
    if(com==0): f1.write("com\tSI\tIC\tPC\tCS\n{}\t{:6.4f}\t{:6.4f}\t{:6.4f}\t{:6.4f}\n".format(com, SI, IC, PC, CS))    
    else: f1.write("{}\t{:6.4f}\t{:6.4f}\t{:6.4f}\t{:6.4f}\n".format(com, SI, IC, PC, CS))    
    f1.close()
    
    col = ['com', 'SI', 'IC', 'PC', 'CS', 'ind', 'updrate', 'updfreq', 'Mat', 'Mname'] + ['U' + str(i) for i in range(NUM_U)]    
    resDF = pd.DataFrame(columns=col)
    # store agents
    for i in range(len(pop)):
        reslist = [com, SI, IC, PC, CS, i, pop[i].getupdrate(), pop[i].getupdfreq()]
        # record prodDF
        Mat = ['SP']
        for m in pop[i].getMList():
            prodList = [m]
            for j in range(NUM_U):
                if ('U' + str(j)) in pop[i].getUList(): prodList.append(pop[i].getprodDF().loc[m,'U' + str(j)])
                else: prodList.append(np.nan)
            resDF = resDF.append(pd.DataFrame([reslist + Mat + prodList], columns=col), ignore_index=True)
        # record percDF
        Mat = ['LI']
        for m in pop[i].getMList():
            prodList = [m]
            for j in range(NUM_U):
                if ('U' + str(j)) in pop[i].getUList(): prodList.append(pop[i].getpercDF().loc[m,'U' + str(j)])
                else: prodList.append(np.nan)
            resDF = resDF.append(pd.DataFrame([reslist + Mat + prodList], columns=col), ignore_index=True)
    
    resDF.to_csv(file2, index=False) # store other measures
       
    CS_pair.to_csv(file3, index=False) # store pairwised CS
    CS_offset.to_csv(file4, index=False) # store offset from group mean CS

      
def recTeacher(fileName, teacher):
    """
    record teacher into a csv file
    :param fileName: file name of the teacher
    :param teacher: teacher agent
    """
    col = ['ind', 'updrate', 'updfreq', 'Mat', 'Mname'] + ['U' + str(i) for i in range(NUM_U)]    
    resDF = pd.DataFrame(columns=col)
    reslist = [teacher.getInd(), teacher.getupdrate(), teacher.getupdfreq()]
    # record prodDF
    Mat = ['SP']
    for m in teacher.getMList():
        prodList = [m]
        for j in range(NUM_U):
            if ('U' + str(j)) in teacher.getUList(): prodList.append(teacher.getprodDF().loc[m,'U' + str(j)])
            else: prodList.append(np.nan)
        resDF = resDF.append(pd.DataFrame([reslist + Mat + prodList], columns=col), ignore_index=True)
    # record percDF
    Mat = ['LI']
    for m in teacher.getMList():
        percList = [m]
        for j in range(NUM_U):
            if ('U' + str(j)) in teacher.getUList(): percList.append(teacher.getpercDF().loc[m,'U' + str(j)])
            else: percList.append(np.nan)
        resDF = resDF.append(pd.DataFrame([reslist + Mat + percList], columns=col), ignore_index=True)
    # store other measures
    resDF.to_csv(fileName, index=False)


def readTeacher(fileName):
    """
    read a teacher agent
    :param fileName: file name of the teacher
    :return an agent object
    """
    # get data
    dataDF = pd.read_csv(fileName)
    # initialize MList and UList
    MList, UList = ['M' + str(i) for i in range(NUM_M)], ['U' + str(i) for i in range(NUM_U)]
    # initialize teacher
    teacher = Agent(dataDF.ind[0], 'Uniform', NUM_M, MList, NUM_U, UList, 'Linear', dataDF.updrate[0], dataDF.updfreq[0])
    # set prodDF
    SPDF = dataDF.loc[dataDF.Mat=='SP',].reset_index(); SPMat = pd.DataFrame(SPDF.loc[:,UList].values, columns=UList, index=MList)
    teacher.setpercDF(SPMat)
    # set percDF
    LIDF = dataDF.loc[dataDF.Mat=='LI',].reset_index(); LIMat = pd.DataFrame(LIDF.loc[:,UList].values, columns=UList, index=MList)
    teacher.setpercDF(LIMat)
    
    return(teacher)    
    

def selTeacherTestee(direct, runID, com, AgInd):
    """
    select an agent as teacher/testee, and create a teacher/testee agent
    :param direct: directory of the result
    :param runID: run ID
    :param com: number of communication/teaching
    :param AgInd: agent index
    :return an agent object
    """
    # get data
    dataDF = pd.read_csv(os.path.join(direct, str(runID), 'indmat_' + str(com) + '.csv'))
    subDF = dataDF.loc[np.int64(dataDF.ind)==AgInd,].reset_index()
    # initialize MList and UList
    MList, UList = ['M' + str(i) for i in range(NUM_M)], ['U' + str(i) for i in range(NUM_U)]
    # initialize teacher
    ag = Agent(AgInd, 'Uniform', NUM_M, MList, NUM_U, UList, 'Linear', subDF.updrate[0], subDF.updfreq[0])
    # set prodDF
    SPDF = subDF.loc[subDF.Mat=='SP',].reset_index(); SPMat = pd.DataFrame(SPDF.loc[:,UList].values, columns=UList, index=MList)
    ag.setprodDF(SPMat)
    # set percDF
    LIDF = subDF.loc[subDF.Mat=='LI',].reset_index(); LIMat = pd.DataFrame(LIDF.loc[:,UList].values, columns=UList, index=MList)
    ag.setpercDF(LIMat)
    
    return(ag)


def setMeaningDist(semProb_table, com):
    """
    set up semProb based on com and semProb_table
    :param semProb_table: semantic distribution table
    :param com: number of teaching
    
    :return a data frame semProb of semantic distribution
    """
    semset = []
    if com+1 <= semProb_table.keys()[0]: semset = semProb_table[semProb_table.keys()[0]] 
    if com+1 > semProb_table.keys()[len(semProb_table.keys())-1]: semset = semProb_table[semProb_table.keys()[len(semProb_table.keys())-1]]
    else:
        for i in range(len(semProb_table.keys())-1):
            if com+1 >= semProb_table.keys()[i] and com+1 < semProb_table.keys()[i+1]:
                semset = semProb_table[semProb_table.keys()[i+1]]
        
    # set up the list of meaning distribution based on semset
    semProb, curtier = pd.DataFrame(np.zeros([NUM_M, 1])), 0
    semProb.columns = ['Prob']
    for i in range(NUM_M):
        if i <= NUM_M/np.float(len(semset))*(curtier+1): semProb.loc[i,'Prob'] = semset[curtier]
        else: 
            curtier += 1
            semProb.loc[i,'Prob'] = semset[curtier]
            
    semProb.loc[:,'Prob'] = semProb.Prob/np.float(sum(semProb.Prob)) # normalize
    return(semProb)
    
    
def runSim(direct, sim_case, noteacher, teacher1, teacher2, runID, directCom, teachrunID, directTeach):
    """
    run simulation
    :param direct: directory for results
    :param sim_case = 1: random communications (allow self-talk), start from random probability
                    = 2: random communications (no self-talk), start from random probability
                    = 3: learning from one or two teachers having same/different preferences
                    = 4: testing
    :param noteacher: number of teachers (1 or 2)
    :param teacher1: teacher1 agent, for teaching case (sim_case=3)
    :param teacher2: teacher2 agent, for teaching case (sim_case=3)
    :param runID: run ID for com, for testing case (sim_case=4)
    :param directCom: directory of com runs, for testing case (sim_case=4)
    :param teachrunID: run ID for teach, for testing case (sim_case=4)
    :param directTeach: directory of teach runs, for testing case (sim_case=4)
    
    """    
    
    # set random seed
    #seed = random.randint(0, sys.maxint)
    seed = random.randint(0, 4294967294) # for ETS grid machine
    np.random.seed(seed)
    #np.random.seed(0)  # for testing
    f = open(os.path.join(direct,'seed.txt'), 'w'); f.write('seed=' + str(seed) + '\n'); f.close()    
    
    # initialize agents
    if sim_case == 1 or sim_case == 2: # binary communications
        # initialize agents
        pop, MList, UList = [], ['M' + str(i) for i in range(NUM_M)], ['U' + str(i) for i in range(NUM_U)]
        for ind in range(NUM_P):
            if upd_init == 'Homo': updrate, updfreq = updrate_mean, updfreq_mean
            if upd_init == 'Hetero_updrate': updrate, updfreq = np.random.normal(updrate_mean, updrate_std, 1)[0], updfreq_mean
            if upd_init == 'Hetero_updfreq': updrate, updfreq = updrate_mean, np.random.normal(updfreq_mean, updfreq_std, 1)[0]   
            if upd_init == 'Hetero_both': updrate, updfreq = np.random.normal(updrate_mean, updrate_std, 1)[0], np.random.normal(updfreq_mean, updfreq_std, 1)[0]
            if updrate < 0.0: updrate = 0.0
            if updrate > 1.0: updrate = 1.0
            if updfreq < 0.0: updfreq = 0.0
            if updfreq > 1.0: updfreq = 1.0
            pop.append(Agent(ind, 'Uniform', NUM_M, MList, NUM_U, UList, upd_way, updrate, updfreq))
        
        # random communications
        for com in range(NUM_C):
            if com == 0: recRes(0, com, pop, 
                                os.path.join(direct, 'res.txt'), 
                                os.path.join(direct, 'indmat_' + str(com) + '.csv'),
                                os.path.join(direct, 'pairwiseCS_' + str(com) + '.csv'),
                                os.path.join(direct, 'pairwiseOffsetCS_' + str(com) + '.csv'))
            if (com+1) % SAMPLE_FREQ == 0: recRes(0, com+1, pop, 
                                                  os.path.join(direct, 'res.txt'),  # file 1: store measures
                                                  os.path.join(direct, 'indmat_' + str(com+1) + '.csv'), # file 2: store individual matrices 
                                                  os.path.join(direct, 'pairwiseCS_' + str(com+1) + '.csv'), # file 3: store pair-wised communicative success
                                                  os.path.join(direct, 'pairwiseOffsetCS_' + str(com+1) + '.csv')) # file 4: store pair-wised offset from group mean communicative success
            # select speaker and listener
            if sim_case == 1: sp, li = random.choice(range(len(pop))), random.choice(range(len(pop)))    
            if sim_case == 2:    
                sp, li = -1, -1
                while sp == li: sp, li = random.choice(range(len(pop))), random.choice(range(len(pop)))
            # communicate
            meaning = random.choice(pop[sp].getMList()); utterance = pop[sp].speak(meaning) # production
            perceive = pop[li].listen(utterance) # perception
            # feedback
            if meaning == perceive: sucfail = 'success'
            else: sucfail = 'failed'
            # update
            pop[sp].updVal('speaker', sucfail, meaning, utterance)
            pop[li].updVal('listener', sucfail, perceive, utterance)
            
            # broadcast to other listeners
            if sucfail == 'success':
                while True:
                    aff_list = random.sample(range(len(pop)), aff_num)
                    if sp not in aff_list and li not in aff_list: break
                for i in aff_list:
                    # all the other agents as speaker/listener to update their matrices
                    pop[i].updVal('speaker', sucfail, meaning, utterance)
                    pop[i].updVal('listener', sucfail, perceive, utterance)                
            
            # store communication history
            f = open(os.path.join(direct,'comHistory.txt'), 'a+')
            f.write("{}\t{}\t{}\n".format(com, sp, li))
            f.close()
            
    if sim_case == 3: # learn from two teachers having different preferences
        # initialize agents
        pop, MList, UList = [], ['M' + str(i) for i in range(NUM_M)], ['U' + str(i) for i in range(NUM_U)]
        for ind in range(NUM_L):
            if upd_init == 'Homo': updrate, updfreq = updrate_mean, updfreq_mean
            if upd_init == 'Hetero_updrate': updrate, updfreq = np.random.normal(updrate_mean, updrate_std, 1)[0], updfreq_mean
            if upd_init == 'Hetero_updfreq': updrate, updfreq = updrate_mean, np.random.normal(updfreq_mean, updfreq_std, 1)[0]   
            if upd_init == 'Hetero_both': updrate, updfreq = np.random.normal(updrate_mean, updrate_std, 1)[0], np.random.normal(updfreq_mean, updfreq_std, 1)[0]
            if updrate < 0.0: updrate = 0.0
            if updrate > 1.0: updrate = 1.0
            if updfreq < 0.0: updfreq = 0.0
            if updfreq > 1.0: updfreq = 1.0
            pop.append(Agent(ind, 'Uniform', NUM_M, MList, NUM_U, UList, upd_way, updrate, updfreq))
            
        # arbitrary adjust some students' learning abilities (updrate and updfreq)
                
        # semantic probability
        semProb_table = {NUM_T/5*1: [100,30,20,10,5],
                         NUM_T/5*2: [100,50,40,20,10],
                         NUM_T/5*3: [100,70,60,40,20],
                         NUM_T/5*4: [100,90,80,60,40],
                         NUM_T: [100,100,100,100,100]}
        
        # teach
        for com in range(NUM_T):
            if com == 0: recRes(1, com, pop, 
                                os.path.join(direct, 'res.txt'), 
                                os.path.join(direct, 'indmat_' + str(com) + '.csv'),
                                os.path.join(direct, 'pairwiseCS_' + str(com) + '.csv'),
                                os.path.join(direct, 'pairwiseOffsetCS_' + str(com) + '.csv'))
            if (com+1) % SAMPLE_FREQ_T == 0: recRes(1, com+1, pop,
                                                    os.path.join(direct, 'res.txt'),  # file 1: store measures
                                                    os.path.join(direct, 'indmat_' + str(com+1) + '.csv'), # file 2: store individual matrices 
                                                    os.path.join(direct, 'pairwiseCS_' + str(com+1) + '.csv'), # file 3: store pair-wised communicative success
                                                    os.path.join(direct, 'pairwiseOffsetCS_' + str(com+1) + '.csv')) # file 4: store pair-wised offset from group mean communicative success
            
            # select speaker, each agent in pop is listener
            if noteacher == 1: sp = teacher1
            elif noteacher == 2: 
                if random.uniform(0,1) <= 0.5: sp = teacher1
                else: sp = teacher2
            
            # teach
            semProb = setMeaningDist(semProb_table, com)
            meaning = sp.getpercDF().index[sp.roulette(semProb.Prob)] # select according to semProb
            utterance = sp.speak(meaning) # production
            for li in range(len(pop)):
                pop[li].updVal('listener', 'success', meaning, utterance) # update
            
            # random student to student talk (allow self-talk)
            sp, li = random.choice(range(len(pop))), random.choice(range(len(pop)))    
            meaning2 = pop[sp].getpercDF().index[pop[sp].roulette(semProb.Prob)] # select according to semProb could be a different meaning
            utterance2 = pop[sp].speak(meaning2) # production
            perceive = pop[li].listen(utterance2) # perception
            # feedback
            if meaning2 == perceive: sucfail = 'success'
            else: sucfail = 'failed'
            # update
            pop[sp].updVal('speaker', sucfail, meaning2, utterance2)
            pop[li].updVal('listener', sucfail, perceive, utterance2)
            
            # store learning history
            f = open(os.path.join(direct,'learnHistory.txt'), 'a+')
            f.write("{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(com, meaning, utterance, sp, li, meaning2, utterance2, sucfail))
            f.close() 
            
    if sim_case == 4: # test
        # create exam
        # create an examination
        # open-ended perception test: testing all utterances and select the answer based on the maximum probability in a row/column)
        # let the last timepoint of the population in which teacher(s) comes from to set the correct answer based on majority vote
        ag0 = selTeacherTestee(directCom, runID, 0, 0)
        exam = pd.DataFrame(np.zeros([NUM_U, NUM_M]), columns=ag0.getMList(), index=ag0.getUList())
        for agind in range(NUM_P):
            ag = selTeacherTestee(directCom, runID, NUM_C, agind) # select agent in that population
            for utterance in exam.index:
                meaning = ag.listen(utterance, maxVal=True)
                exam.loc[utterance, meaning] += 1   
        exam.to_csv(os.path.join(direct, 'exam_allanswer_teacherpop.csv'), index=True) # index=True to save index
        
        # get correct answer
        exam_answer = pd.DataFrame(np.zeros([NUM_U, 1]), columns=['Answer'], index=ag0.getUList())
        for utterance in exam_answer.index:
            probList = exam.loc[utterance,:]
            exam_answer.loc[utterance, 'Answer'] = exam.columns[probList==np.max(probList)][0]
        # save exam_answers
        exam_answer.to_csv(os.path.join(direct, 'exam_answers.csv'), index=True) # index=True to save index
        
        # get answers from teachers
        if noteacher == 1:
            exam_answer_teacher = pd.DataFrame(np.zeros([NUM_U, 1]), columns=['Answer'], index=ag0.getUList())
            for utterance in exam_answer.index:
                exam_answer_teacher.loc[utterance, 'Answer'] = teacher1.listen(utterance, maxVal=True)
            # save exam_answers
            exam_answer_teacher.to_csv(os.path.join(direct, 'exam_answers_teacher.csv'), index=True) # index=True to save index
        if noteacher == 2:
            exam_answer_teacher1 = pd.DataFrame(np.zeros([NUM_U, 1]), columns=['Answer'], index=ag0.getUList())
            exam_answer_teacher2 = pd.DataFrame(np.zeros([NUM_U, 1]), columns=['Answer'], index=ag0.getUList())
            for utterance in exam_answer.index:
                exam_answer_teacher1.loc[utterance, 'Answer'] = teacher1.listen(utterance, maxVal=True)
                exam_answer_teacher2.loc[utterance, 'Answer'] = teacher2.listen(utterance, maxVal=True)
            # save exam_answers
            exam_answer_teacher1.to_csv(os.path.join(direct, 'exam_answers_teacher1.csv'), index=True) # index=True to save index
            exam_answer_teacher2.to_csv(os.path.join(direct, 'exam_answers_teacher2.csv'), index=True) # index=True to save index            
        
        # use the exam to text students performance
        for com in range(0, NUM_T+1, SAMPLE_FREQ_T):
            print "test after {} learning".format(com)
            answer_sheet = pd.DataFrame(np.zeros([NUM_L, NUM_U]), columns=ag0.getUList(), index=['Ag' + str(x) for x in range(NUM_L)]) # set up answer_sheet for each agent's score
            for agind in range(NUM_L):
                ag = selTeacherTestee(directTeach, teachrunID, com, agind) # select learner in that population
                for utterance in answer_sheet.columns:
                    answer = exam_answer.loc[utterance, 'Answer'] # correct answer
                    #meaning = ag.listen(utterance, maxVal=True) # student's answer based on maximum probability
                    meaning = ag.listen(utterance) # student's answer based on probability
                    if answer == meaning: answer_sheet.loc['Ag' + str(agind), utterance] = 1                       
            fileName = os.path.join(direct, 'answer_sheet_' + str(com) + '.csv')
            answer_sheet.to_csv(fileName, index=True) # index=True to save index

        # record students' individual differences
        ind_feature = pd.DataFrame(np.zeros([NUM_L, 2]), columns=['UpdRate', 'UpdFreq'], index=['Ag' + str(x) for x in range(NUM_L)])
        com = 0
        for agind in range(NUM_L):
            ag = selTeacherTestee(directTeach, teachrunID, com, agind)
            ind_feature.loc['Ag' + str(agind), 'UpdRate'] = ag.getupdrate()
            ind_feature.loc['Ag' + str(agind), 'UpdFreq'] = ag.getupdfreq()
        fileName = os.path.join(direct, 'ind_feature.csv')
        ind_feature.to_csv(fileName, index=True) # index=True to save index


def MainFunc(direct, totalrun, sim_case, noteacher, teacher1, teacher2, runID, directCom, teachrunID, directTeach):
    """
    main function
    :param direct: directory for results
    :param totalrun: total number of runs in this condition
    :param sim_case = 1: random communications (allow self-talk), start from random probability
                    = 2: random communications (no self-talk), start from random probability
                    = 3: learning from two teachers having different preference
                    = 4: testing
    :param noteacher: number of teachers (1 or 2), for teaching case (sim_case=3)
    :param teacher1: teacher1 agent, for teaching case (sim_case=3)
    :param teacher2: teacher2 agent, for teaching case (sim_case=3)
    :param runID: run ID for com, for testing case (sim_case=4)
    :param directCom: directory of com runs, for testing case (sim_case=4)
    :param teachrunID: run ID for teach, for testing case (sim_case=4)
    :param directTeach: directory of teach runs, for testing case (sim_case=4)
    
    """    
    for run in range(totalrun):
        if sim_case == 1 or sim_case == 2: print "Run = ", run+1
        if sim_case == 3: print "Teach = ", run+1
        if sim_case == 4: print "Test = ", run+1
        dir = os.path.join(direct, str(run+1))     
        try: os.stat(dir)
        except: os.mkdir(dir)
        runSim(dir, sim_case, noteacher, teacher1, teacher2, runID, directCom, teachrunID, directTeach)



######
# functions for drawing results, select teachers and others
######
def drawRes(sim_case, direct, runID, fileName):
    """
    draw res.txt from a particular run
    :param sim_case = 1: random communications (allow self-talk), start from random probability
                    = 2: random communications (no self-talk), start from random probability
                    = 3: learning from two teachers having different preference
                    = 4: testing
    :param direct: directory of the result
    :param runID: run ID
    :param fileName: figure file name
    """
    resDF = pd.read_csv(os.path.join(direct, str(runID), 'res.txt'), sep="\t")
    if sim_case == 1 or sim_case == 2: xlabel, xtick = 'No. Communications per Agent', [str(com/(1000.0*NUM_P)) + 'e3' for com in resDF.com]
    if sim_case == 3 or sim_case == 4: xlabel, xtick = 'No. Learning per Agent', [str(com) for com in resDF.com]
    ylabel = 'Index Value'
    
    # draw figure
    # SI
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    ax.plot(resDF.com, resDF.SI); ax.legend(['SI'], loc='upper left')
    ax.set_ylim([0.9,1.0]); ax.grid(True)
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(resDF.com, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_SI.png', orientation = 'landscape', dpi=300); plt.close(fig)
    
    # IC and PC
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    ax.plot(resDF.com, resDF.IC); ax.plot(resDF.com, resDF.PC); ax.legend(['IC', 'PC'], loc='upper left')
    ax.set_ylim([0.0,1.0]); ax.grid(True)    
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(resDF.com, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_IC_PC.png', orientation = 'landscape', dpi=300); plt.close(fig)        
 
    # CS
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    ax.plot(resDF.com, resDF.CS); ax.legend(['CS'], loc='upper left')
    ax.set_ylim([0.0,1.0]); ax.grid(True)  
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(resDF.com, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_CS.png', orientation = 'landscape', dpi=300); plt.close(fig) 


def drawHeatMU(pktype, maptype, timepoint, fileName, ProdMap, PercMap):
    """
    draw heatmap based on M-U matrix
    :param pktype: use which package to draw heatmap: 'plt', 'seaborn' or 'plotly'
    :param maptype: type of drawing: 'Prod': production matrix only; 'Perc': perception matrix only
    :param timepoint: time point
    :param fileName: file name header for the result figure/video
    :param ProdMap: production matrix
    :param PercMap: perception matrix
    """
    if pktype == 'plt':
        import matplotlib.ticker as ticker
        import matplotlib.cm as cm
        import matplotlib as mpl
    
    if pktype == 'seaborn':
        import seaborn as sns
        import matplotlib.cm as cm
        
    if pktype == 'plotly':
        import plotly
        plotly.offline.init_notebook_mode() # run at the start of every ipython notebook
        import plotly.plotly as py
        import plotly.io as pio
        import plotly.graph_objs as go
        from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
    
    if maptype == 'Prod': 
        FinalFileName = fileName + '_Prod_' + str(timepoint)
        title = 'Production Matrix at ' + str(timepoint)        
        
        if pktype == 'plt':
            fig = plt.figure(); fig, ax = plt.subplots(1,1, figsize=(12,12))
            heatplot = ax.imshow(ProdMap, cmap='BuPu')
            ax.set_xticklabels(ProdMap.columns); ax.set_yticklabels(ProdMap.index)
            tick_spacing = 1
            ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            ax.yaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            ax.set_title(title)
            # ax.set_xlabel('Year'); ax.set_ylabel('Month')
            plt.show(); plt.savefig(FinalFileName + '.png', orientation = 'landscape', dpi=300); plt.close(fig) 
        
        if pktype == 'seaborn':
            fig = plt.figure(figsize = (12,12))
            r = sns.heatmap(ProdMap, cmap='BuPu', vmax=1.0, vmin=0.0)
            r.set_title(title)
            plt.show(); plt.savefig(FinalFileName + '.png', orientation = 'landscape', dpi=300); plt.close(fig) 
            
        if pktype == 'plotly':
            data = [go.Heatmap(z = ProdMap.values, x = ProdMap.columns, y = ProdMap.index)]
            layout = go.Layout(title = title, xaxis = dict(ticks='', nticks=NUM_M), yaxis = dict(ticks='', nticks=NUM_U))
            fig = go.Figure(data=data, layout=layout)
            plot(fig, filename=FinalFileName + '.html')
            #plot(fig, image = 'png', image_filename = FinalFileName)
    
    if maptype == 'Perc': 
        FinalFileName = fileName + '_Perc_' + str(timepoint)
        title = 'Perception Matrix at ' + str(timepoint)
        
        if pktype == 'plt':
            fig = plt.figure(); fig, ax = plt.subplots(1,1, figsize=(12,12))
            heatplot = ax.imshow(PercMap, cmap='BuPu')
            ax.set_xticklabels(PercMap.columns); ax.set_yticklabels(PercMap.index)
            tick_spacing = 1
            ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            ax.yaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            ax.set_title(title)
            # ax.set_xlabel('Year'); ax.set_ylabel('Month')
            plt.show(); plt.savefig(FinalFileName + '.png', orientation = 'landscape', dpi=300); plt.close(fig) 
        
        if pktype == 'seaborn':
            fig = plt.figure(figsize = (12,12))
            r = sns.heatmap(PercMap, cmap='BuPu', vmax=1.0, vmin=0.0)
            r.set_title(title)
            plt.show(); plt.savefig(FinalFileName + '.png', orientation = 'landscape', dpi=300); plt.close(fig) 
            
        if pktype == 'plotly':
            data = [go.Heatmap(z = PercMap.values, x = PercMap.columns, y = PercMap.index)]
            layout = go.Layout(title = title, xaxis = dict(ticks='', nticks=NUM_M), yaxis = dict(ticks='', nticks=NUM_U))
            fig = go.Figure(data=data, layout=layout)
            plot(fig, filename=FinalFileName + '.html')
            #plot(fig, image = 'png', image_filename = FinalFileName)
    

def drawMUmap(pktype, case, maptype, direct, runID, fileName):
    """
    draw M-U matrics (production and/or perception matrics)
    :param pktype: use which package to draw heatmap: 'plt' or 'plotly'
    :param case: 'Com': communication; 'Teach': teach/learn
    :param maptype: type of drawing: 'Prod': production matrix only; 'Perc': perception matrix only
    :param direct: directpry of the result
    :param runID: run ID
    :param fileName: file name header for the result figure/video
    """
    # set sampling time points
    if case == 'Com': time, agind = range(0, NUM_C+1, SAMPLE_FREQ), range(NUM_P)
    if case == 'Teach': time, agind = range(0, NUM_T+1, SAMPLE_FREQ_T), range(NUM_L)
    
    for timepoint in time:
        resDF = pd.read_csv(os.path.join(direct, str(runID), 'indmat_' + str(timepoint) + '.csv'))
        if maptype == 'Prod':
            ProdMap = pd.DataFrame(np.zeros([NUM_M, NUM_U]), columns = ['U' + str(x) for x in range(NUM_U)], index = ['M' + str(x) for x in range(NUM_M)])
            for ag in agind:
                res = resDF.loc[(resDF.ind==ag) & (resDF.Mat=='SP'), ['U' + str(x) for x in range(NUM_U)]]
                res.index = ['M' + str(x) for x in range(NUM_M)]
                ProdMap += res
            ProdMap /= np.float64(len(agind)) # get average MUmap            
            drawHeatMU(pktype, maptype, timepoint, fileName, ProdMap, None) # draw heatmap            
            
        if maptype == 'Perc':
            PercMap = pd.DataFrame(np.zeros([NUM_M, NUM_U]), columns = ['U' + str(x) for x in range(NUM_U)], index = ['M' + str(x) for x in range(NUM_M)])
            for ag in agind:
                res = resDF.loc[(resDF.ind==ag) & (resDF.Mat=='LI'), ['U' + str(x) for x in range(NUM_U)]]
                res.index = ['M' + str(x) for x in range(NUM_M)]
                PercMap += res
            PercMap /= np.float64(len(agind)) # get average MUmap             
            drawHeatMU(pktype, maptype, timepoint, fileName, None, PercMap) # draw heatmap            
    
#    if pktype != 'plotly':
#        import subprocess
#        # mencoder mf://*.png -mf type=png:w=800:h=600:fps=25 -ovc lavc -lavcopts vcodec=mpeg4 -oac copy -o output.avi
#        command = ('mencoder', 'mf://*.png', '-mf', 'type=png:w=800:h=600:fps=25', '-ovc', 'lavc', '-lavcopts',
#           'vcodec=mpeg4', '-oac', 'copy', '-o', fileName + '.avi')
#        #os.spawnvp(os.P_WAIT, 'mencoder', command)
#        print("\n\nabout to execute:\n%s\n\n" % ' '.join(command))
#        subprocess.check_call(command)
#        print("\n\n The movie was written to {}.avi".format(fileName))
#        print("\n\n You may want to delete *.png.\n")
    

def drawScore(direct, runID, fileName):
    """
    draw score data from a particular teach run
    :param direct: directory of the result
    :param runID: run ID
    :param fileName: figure file name
    """
    testTime = range(0, NUM_T+1, SAMPLE_FREQ_T)
    xlabel, ylabel, xtick = 'No. Learning per Agent', 'Score', [str(com) for com in testTime]
    
    # score of each leaner
    allScore_byLearner = pd.DataFrame(np.zeros([len(testTime), NUM_L]), columns = ['Ag' + str(x) for x in range(NUM_L)], index=testTime)
    allScore_byLearner['AvgScore'] = None
    for com in testTime:
        resDF = pd.read_csv(os.path.join(direct, str(runID), 'answer_sheet_' + com + '.csv'))
        for agind in resDF.index:
            allScore_byLearner.loc[com, agind] = sum(resDF.loc[agind, :])
        allScore_byLearner.loc[com, 'AvgScore'] = pd.mean(allScore_byLearner.loc[com, resDF.index])      
    allScore_byLearner.to_csv(fileName + '_Learner.csv', index=True)
    
    # score of each leaner
    allScore_byItem = pd.DataFrame(np.zeros([len(testTime), NUM_L]), columns = ['U' + str(x) for x in range(NUM_U)], index=testTime)
    allScore_byItem['AvgScore'] = None
    for com in testTime:
        resDF = pd.read_csv(os.path.join(direct, str(runID), 'answer_sheet_' + com + '.csv'))
        for item in resDF.columns:
            allScore_byItem.loc[com, item] = sum(resDF.loc[:, item])
        allScore_byItem.loc[com, 'AvgScore'] = pd.mean(allScore_byItem.loc[com, resDF.columns])      
    allScore_byItem.to_csv(fileName + '_Item.csv', index=True)
    
    # draw figure
    # score by learner
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    for agind in allScore_byLearner.columns[len(allScore_byLearner.columns)-1]:
        ax.plot(allScore_byLearner.index, allScore_byLearner.loc[:,agind])
    ax.legend(allScore_byLearner.columns[len(allScore_byLearner.columns)-1], loc='upper left')
    ax.set_ylim([0.0, NUM_U]); ax.grid(True)  
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(testTime, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_Learner.png', orientation = 'landscape', dpi=300); plt.close(fig) 
    
    # avg score of all learners
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    ax.plot(avgScore_byLearner.index, avgScore_byLearner.AvgScore); ax.legend(['Average Score'], loc='upper left')
    ax.set_ylim([0.0, NUM_U]); ax.grid(True)
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(testTime, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_Learner_avg.png', orientation = 'landscape', dpi=300); plt.close(fig) 

    # score by item
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    for item in allScore_byItem.columns[len(allScore_byItem.columns)-1]:
        ax.plot(allScore_byItem.index, allScore_byItem.loc[:,item])
    ax.legend(allScore_byItem.columns[len(allScore_byItem.columns)-1], loc='upper left')
    ax.set_ylim([0.0, NUM_L]); ax.grid(True)  
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(testTime, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_Item.png', orientation = 'landscape', dpi=300); plt.close(fig) 
    
    # avg score of all learners
    fig = plt.figure(); fig.set_size_inches(20, 10); ax = fig.add_subplot(111); fontsize = 20
    ax.plot(avgScore_byItem.index, avgScore_byItem.AvgScore); ax.legend(['Average Score'], loc='upper left')
    ax.set_ylim([0.0, NUM_L]); ax.grid(True)
    ax.set_xlabel(xlabel, fontsize=fontsize); ax.set_ylabel(ylabel, fontsize=fontsize)
    plt.xticks(testTime, xtick, rotation=45, fontsize=fontsize)
    plt.show(); plt.savefig(fileName + '_Item_avg.png', orientation = 'landscape', dpi=300); plt.close(fig) 



# Overall functions

## Use local PC
## 1) to run random communications, use the following lines
#direct = os.path.join(wd, 'com')
#try: os.stat(direct)
#except: os.mkdir(direct)
#MainFunc(direct, NUM_RUN, 1, 0, None, None, None, None, None, None) # allow self-talk
##MainFunc(direct, NUM_RUN, 2, 0, None, None) # no self-talk
## draw measures
#%pylab
#pktype = 'seaborn'
##pktype = 'plt'
##pktype = 'plotly'
#for i in range(1,NUM_RUN+1):
#    drawRes(1, direct, i, os.path.join(direct, str(i), 'res'))
#    drawMUmap(pktype, 'Com', 'Prod', direct, i, os.path.join(direct, str(i), 'Com' + str(i) + '_MUheatmap'))
#    drawMUmap(pktype, 'Com', 'Perc', direct, i, os.path.join(direct, str(i), 'Com' + str(i) + '_MUheatmap'))
#
#
# 2) to run teaching and testing, use the following lines
## for one teacher case
#direct = os.path.join(wd, 'com')
#noteacher = 1; runID, numcom1, AgInd1 = 1, 72000, 30; teacher1 = selTeacherTestee(direct, runID, numcom1, AgInd1)
#direct = os.path.join(wd, 'teach' + '_com' + str(runID))
#try: os.stat(direct)
#except: os.mkdir(direct)
## save teacher
#recTeacher(os.path.join(direct, "Teacher.csv"), teacher1)
#MainFunc(direct, NUM_RUN, 3, noteacher, teacher1, None, None, None, None, None) # teaching
## draw measures
#%pylab
#direct = os.path.join(wd, 'teach' + '_com' + str(runID))
#for i in range(1,NUM_RUN+1):
#    drawRes(3, direct, i, os.path.join(direct, str(i), 'res'))
#    
## for two teachers case
#direct = os.path.join(wd, 'com')
#noteacher = 2
#runID, numcom1, AgInd1 = 1, 72000, 30; teacher1 = selTeacherTestee(direct, runID, numcom1, AgInd1)
#runID, numcom2, AgInd2 = 1, 72000, 19; teacher2 = selTeacherTestee(direct, runID, numcom2, AgInd2)
#direct = os.path.join(wd, 'teach' + '_com' + str(runID))
#try: os.stat(direct) 
#except: os.mkdir(direct)
## save teacher
#recTeacher(os.path.join(direct, "Teacher1.csv"), AgInd1, teacher1)
#recTeacher(os.path.join(direct, "Teacher2.csv"), AgInd2, teacher2)
#MainFunc(direct, NUM_RUN, 3, noteacher, teacher1, teacher2, None, None, None, None) # teaching
## draw measures
#%pylab
#direct = os.path.join(wd, 'teach' + '_com' + str(runID))
#for i in range(1,NUM_RUN+1):
#    drawRes(3, direct, i, os.path.join(direct, str(i), 'res'))
#
#
## 3) to run testing
#runID, directCom = 1, os.path.join(wd, 'com') 
#directTeach = os.path.join(wd, 'teach' + '_com' + str(runID))
#
## for one teacher case
#noteacher = 1; teacher1 = readTeacher(os.path.join(directTeach, "Teacher.csv"))
#for i in range(1,11):
#    direct = os.path.join(wd, 'test' + '_com' + str(runID) + '_teach' + str(i))
#    try: os.stat(direct) 
#    except: os.mkdir(direct)
#    MainFunc(direct, NUM_TEST, 4, noteacher, teacher1, None, runID, directCom, i, directTeach)
## draw measures
#%pylab
#for i in range(1,11):
#    direct = os.path.join(wd, 'test' + '_com' + str(runID) + '_teach' + str(i))
#    drawScore(direct, 1, os.path.join(direct, NUM_TEST, 'sc'))
##
## for two teachers case
#noteacher = 2; teacher1, teacher2 = readTeacher(os.path.join(directTeach, "Teacher1.csv")), readTeacher(os.path.join(directTeach, "Teacher2.csv"))
#for i in range(1,11):
#    direct = os.path.join(wd, 'test' + '_com' + str(runID) + '_teach' + str(i))
#    try: os.stat(direct) 
#    except: os.mkdir(direct)
#    MainFunc(direct, NUM_TEST, 4, noteacher, teacher1, teacher2, runID, directCom, i, directTeach)
## draw measures
#%pylab
#for i in range(1,11):
#    direct = os.path.join(wd, 'test' + '_com' + str(runID) + '_teach' + str(i))
#    drawScore(direct, 1, os.path.join(direct, NUM_TEST, 'sc'))


# Use grid machine
def main(argv):
    """
    Main function for grid machine usage
    :param argv[0]: sim_case
    :param argv[1]: run index
    if sim_case == 3:
        :param argv[2]: noteacher (1)
        :param argv[3]: runID: communication run ID
        :param argv[4]: numcom: number of communication
        :param argv[5]: teacher agent index
    if sim_case == 4:
        :param argv[2]: noteacher (1)
        :param argv[3]: runID
        :param argv[4]: teachrunID
    """
    sim_case, run = np.int(argv[0]), np.int(argv[1])
    noteacher, teacher1, teacher2, runID, directCom, teachrunID, directTeach = 0, None, None, None, None, None, None
    direct = None
    
    if sim_case == 1 or sim_case == 2: 
        print "Run = ", run        
        direct = os.path.join(wd, 'com')
        try: os.stat(direct)
        except: os.mkdir(direct)
    
    if sim_case == 3: 
        noteacher, runID, numcom1, AgInd1 = np.int(argv[2]), np.int(argv[3]), np.int(argv[4]), np.int(argv[5])
        teacher1 = selTeacherTestee(os.path.join(wd, 'com'), runID, numcom1, AgInd1)       
        print "Teach = ", run
        direct = os.path.join(wd, 'teach' + '_com' + str(runID))
        try: os.stat(direct)
        except: os.mkdir(direct)
        # save teacher
        recTeacher(os.path.join(direct, "Teacher.csv"), teacher1)

    if sim_case == 4:
        noteacher = np.int(argv[2]); teacher1 = readTeacher(os.path.join(directTeach, "Teacher.csv"))
        runID, directCom, teachrunID = np.int(argv[3]), os.path.join(wd, 'com'), np.int(argv[4]) 
        directTeach = os.path.join(wd, 'teach' + '_com' + str(runID))        
        print "Test = ", runID
        direct = os.path.join(wd, 'test' + '_com' + str(runID) + '_teach' + str(teachrunID))
        try: os.stat(direct) 
        except: os.mkdir(direct)
    
    dir = os.path.join(direct, str(run))
    try: os.stat(dir)
    except: os.mkdir(dir)
    runSim(dir, sim_case, noteacher, teacher1, teacher2, runID, directCom, teachrunID, directTeach)

    
if __name__ == '__main__':
    main(sys.argv[1:])