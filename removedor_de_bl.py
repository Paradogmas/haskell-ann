f = open("W22.txt", "r")
i = 0
string = ""
numero_de_linhas = 50
while i < numero_de_linhas:
    string += f.readline()
    if (i+1)%5 != 0:
        string = string[:-1]
    i+=1

escrever = open("W21.txt", "w")
escrever.write(string)