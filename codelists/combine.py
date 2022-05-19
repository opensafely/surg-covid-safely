import sys
import os
import csv



def main():

    # #
    # with open('opensafely-cancer-excluding-lung-and-haematological-snomed.csv', mode ='r')as file:
    #   csvreader = csv.reader(file)
    #   next(csvreader)
    #   cancer_excluding = list(csvreader)
      
    # with open('opensafely-lung-cancer-snomed.csv', mode ='r')as file:
    #     csvreader = csv.reader(file)
    #     next(csvreader)
    #     lung = list(csvreader)
        
    # with open('opensafely-haematological-cancer-snomed.csv', mode ='r')as file:
    #    csvreader = csv.reader(file)
    #    next(csvreader)
    #    haemat = list(csvreader)

    # cancer_excluding = set([tuple(i) for i in cancer_excluding])
    # lung = set([tuple(i) for i in lung])
    # haemat = set([tuple(i) for i in haemat])
    
    
    # interim = cancer_excluding.union(lung)
    # output_set = interim.union(haemat)
    
    # #
    # with open('cancer-snomed.csv','w', newline='') as f:
    #     writer = csv.writer(f, delimiter=',')
    #     writer.writerows(output_set)
    
    with open('ciaranmci-surgery-covidsurg-replication-excluding-exclusions-5c09dd62.csv', mode ='r')as file:
      csvreader = csv.reader(file)
      next(csvreader)
      original_codelist = list(csvreader)
      
    with open('Abbott SNOMED CT codes to add.csv', mode ='r')as file:
        csvreader = csv.reader(file)
        next(csvreader)
        Abbott_codes = list(csvreader)
        
    original_codelist_set = set([tuple(i) for i in original_codelist])
    Abbott_codes_set = set([tuple(i) for i in Abbott_codes])
    
    
    output_set = original_codelist_set.union(Abbott_codes_set)

    #
    with open('updated-surgery-codelist.csv','w', newline='') as f:
        writer = csv.writer(f, delimiter=',')
        writer.writerows(output_set)


if __name__ == ('__main__'):
    
    sys.exit(main())
