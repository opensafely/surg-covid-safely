import sys
import os
import csv



def main():

    #
    with open('opensafely-cancer-excluding-lung-and-haematological-snomed.csv', mode ='r')as file:
      csvreader = csv.reader(file)
      next(csvreader)
      cancer_excluding = list(csvreader)
      
    with open('opensafely-lung-cancer-snomed.csv', mode ='r')as file:
        csvreader = csv.reader(file)
        next(csvreader)
        lung = list(csvreader)
        
    with open('opensafely-haematological-cancer-snomed.csv', mode ='r')as file:
       csvreader = csv.reader(file)
       next(csvreader)
       haemat = list(csvreader)

    cancer_excluding = set([tuple(i) for i in cancer_excluding])
    lung = set([tuple(i) for i in lung])
    haemat = set([tuple(i) for i in haemat])
    
    
    interim = cancer_excluding.union(lung)
    output_set = interim.union(haemat)
    
    #
    with open('cancer-snomed.csv','w', newline='') as f:
        writer = csv.writer(f, delimiter=',')
        writer.writerows(output_set)

if __name__ == ('__main__'):
    
    sys.exit(main())
