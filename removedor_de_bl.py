f = open("nome_de_entrada.txt", "r")
i = 0
string = ""
numero_de_linhas = 0
while i < numero_de_linhas:
    string += f.readline()
    if (i+1)%16 != 0:
        print('q')
        string = string[:-1]
    i+=1

escrever = open("nome_de_saida.txt", "w")
escrever.write(string)