'''
Created on Mar 16, 2016

@author: achaluv
'''
import csv
from collections import defaultdict

def getVerticesAndAttributes():
    fileDes = open("fb_caltech_small_attrlist.csv",'r')
    data = list(csv.reader(fileDes))
    vertAttrVec = defaultdict(list)
    
    for i in range(1,len(data)):
        vertAttrVec[i]=[]
    
    for i in range(1,len(data)):
        for attr in data[i]:
            vertAttrVec[i].append(attr)
            
    fileDes.close()
    return vertAttrVec
    
def getEdgesDict():
    fileDes = open("fb_caltech_small_edgelist.txt",'r')
    vertexEdges = defaultdict(list)
    for line in fileDes:
        vertices = line.strip('\n').split(' ')
        vertexEdges[vertices[0]].append(vertices[1])
    fileDes.close()
    return vertexEdges
    
def main():
    #Procure the graph properties
    vertexAttributes = getVerticesAndAttributes()
    vertexEdges = getEdgesDict()
    
    
    
if __name__ == '__main__':
    main()