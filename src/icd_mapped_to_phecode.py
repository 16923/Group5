import numpy as np
import pandas as pd
df=pd.read_csv("data1-1_temp.csv")

class Phemap:

    def __init__(self, source_file, mapping_file):
        cols_phecodes = [
            'phecode',
            'phenotype',
            'phecode_exclude_range',
            'sex',
            'rollup',
            'leaf',
            'category_number',
            'category']

        dtypes_phecodes = {x: 'str' for x in cols_phecodes}

        cols_phecodemap = [
            'icd10',
            'phecode',
            'phecode_exclude_range',
            'phenotype_exlude']

        dtypes_map = {x: 'str' for x in cols_phecodemap}

        self.phecodes = pd.read_csv(
            source_file,
            dtype=dtypes_phecodes,
            names=cols_phecodes,
            header=0)

        self.phecodes['phecode_num'] = pd.to_numeric(
            self.phecodes['phecode'])

        self.phecodemap = pd.read_csv(
            mapping_file,
            dtype=dtypes_map,
            names=cols_phecodemap,
            header=0)

        self.phecodemap['phecode_num'] = pd.to_numeric(
            self.phecodemap['phecode'])

    def get_phecode_info(self, phecode):

        m = self.phecodes['phecode_num'] == float(phecode)
        matches = self.phecodes.loc[m].to_dict(orient='records')

        if matches:
            return matches[0]
        else:
            raise ValueError("Phecode not found: %s" % phecode)

    def get_phecode_for_icd10(self, icd10):

        m = self.phecodemap['icd10'] == icd10
        phecode_match = self.phecodemap.loc[m].to_dict(orient='records')

        if phecode_match:
            return [p['phecode'] for p in phecode_match]
        else:
            raise ValueError("Mapping for term not found: %s" % icd10)

    def get_icd_for_phecode(self, phecode):


        m = self.phecodemap['phecode_num'] == float(phecode)
        phecode_match = self.phecodemap.loc[m]

        if len(phecode_match) > 0:
            return phecode_match.icd10.tolist()
        else:
            raise ValueError("No map found for phecode %s" % phecode)

    def get_phecode_exclusions(self, phecode):


        phecode = self.get_phecode_info(phecode)
        excl_range = phecode['phecode_exclude_range']
        (ex_start, ex_end) = excl_range.split('-')

        phecodes_in_exclude_range = self.phecodes[
            (self.phecodes['phecode_num'] >= float(ex_start)) &
            (self.phecodes['phecode_num'] <= float(ex_end))]

        v = phecodes_in_exclude_range.phecode.tolist()
        return v

    def get_all_phecodes(self):
        return self.phecodes.to_dict(orient='records')


source_file = '\extraction_and_recoding\scripts\phemap-master\data\phecode_definitions1.2.csv'
mapping_file = '\extraction_and_recoding\scripts\phemap-master\data\phecode_map_v1_2_icd10_beta.csv'

phemap = Phemap(source_file=source_file, mapping_file=mapping_file)

ICD_general = []
ICD_secondary = []

df2 = df.iloc[:, [84,85,86,87,88,89]]
df3 = df.iloc[:, [90,91,92,93,94,95]]
l2 = df2.values.tolist()
l3 = df3.values.tolist()



icd10 = pd.read_csv(mapping_file)
icd10 = icd10.iloc[:, [0]]
icd10 = icd10.values.tolist()

icd10= eval('[%s]'%repr(icd10).replace('[', '').replace(']', ''))

l2_new = []
l3_new = []

for i in range(0,len(l3)):
    temp = []
    for j in range(0,len(l3[i])):
        item = l3[i][j]
        print(item)
        if str(item) == "nan":
            print("0")
        elif item in icd10:
            temp.append(phemap.get_phecode_for_icd10(item)[0])
        elif (str(item)[:-1]+"."+str(item)[-1]) in icd10:
            seg = (str(item)[:-1]+"."+str(item)[-1])
            temp.append (phemap.get_phecode_for_icd10(seg)[0])
        else:
            print(1)
    temp = list(set(temp))
    l3_new.append(temp)

for i in range(0,len(l2)):
    temp = []
    for j in range(0,len(l2[i])):
        item = l2[i][j]
        print(item)
        if str(item) == "nan":
            print("0")
        elif item in icd10:
            temp.append(phemap.get_phecode_for_icd10(item)[0])
        elif (str(item)[:-1]+"."+str(item)[-1]) in icd10:
            seg = (str(item)[:-1]+"."+str(item)[-1])
            temp.append(phemap.get_phecode_for_icd10(seg)[0])
        else:
            print(1)
    temp = list(set(temp))
    l2_new.append(temp)

df["icd_general"] = l2_new
df["icd_secondary"] = l3_new
asthma_secondary = []
asthma_general = []
for item in l2_new:
    if "495" in item or "495.2" in item:
        asthma_secondary.append(1)
    else:
        asthma_secondary.append(0)
for item in l3_new:
    if "495" in item or "495.2" in item:
        asthma_general.append(1)
    else:
        asthma_general.append(0)
df["icd_asthma_general"] = asthma_general
df["icd_asthma_secondary"] = asthma_secondary
df.to_csv(r"icd_embedded_temp.csv",index=False)
#then manually remove nan and [] brackets


