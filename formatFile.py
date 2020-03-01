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