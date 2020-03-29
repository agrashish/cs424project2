# just add the header and clean some whitespaces in each file
# since data for each hurricane can span multiple lines, 
# we will make the hurricane identifiying columns added to every single row that belonngs to each hurricane
# that's pretty much all our preprocessing does


# preprocessing for atlantic data
with open("hurdat2-1851-2018-120319.txt", 'r') as f:
    with open("hurdat2-formatted.txt", 'w') as o:
        header = "Hurricane,Name,NumEntries,Date,Time,RecordID,Status,Lat,Long,MaxWind,MinPress,"
        header += "WRNE34,WRSE34,WRSW34,WRNW34,wRNE50,WRSE50,WRSW50,WRNW50,WRNE64,WRSE64,WRSW64,WRNW64,\n"
        o.write(header)
        curHeader = ""
        for line in f:
            if(len(line.split(",")) <= 4):
                curHeader = line.rstrip()
            else:
                o.write(curHeader + line)

# preprocessing for pacific data            
with open("hurdat2-nepac-1949-2018-122019.txt", 'r') as f:
    with open("hurdat2Pacific-formatted.txt", 'w') as o:
        header = "Hurricane,Name,NumEntries,Date,Time,RecordID,Status,Lat,Long,MaxWind,MinPress,"
        header += "WRNE34,WRSE34,WRSW34,WRNW34,wRNE50,WRSE50,WRSW50,WRNW50,WRNE64,WRSE64,WRSW64,WRNW64,\n"
        o.write(header)
        curHeader = ""
        for line in f:
            if(len(line.split(",")) <= 4):
                curHeader = line.rstrip()
            else:
                o.write(curHeader + line)