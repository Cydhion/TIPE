import csv
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.image as mpimg

f = open('map.csv')

fichiercsv = csv.reader(f)

listeownership = []
listegarrison = []
listeindustry = []
listeterrain = []
listelink = [] 

for ligne in fichiercsv:
    listeownership.append(ligne[0])
    listegarrison.append(ligne[1])
    listeindustry.append(ligne[2])
    listeterrain.append(ligne[3])
    listelink.append(ligne[4:])

G = nx.Graph()

taille = len(listeownership)

G.add_nodes_from(list(range(taille)))

for i in range(taille): #Links
    for j in range(len(listelink[i])):
        G.add_edges_from([(i,int(listelink[i][j]))])

tabcolor = ['white'] * len(G.nodes())

for node in G.nodes(): #Couleur
    if (listeterrain[node] == 'W') :
        tabcolor[node] = 'skyblue'      
    elif (listeownership[node] == '-1' and listeterrain[node] != 'W') :
        tabcolor[node] = 'yellow' 
    elif listeownership[node] == '0' :
        tabcolor[node] = 'blue' 
    elif listeownership[node] == '1' :
        tabcolor[node] = 'green' 
    elif listeownership[node] == '2' :
        tabcolor[node] = 'gray' 
    elif listeownership[node] == '3' :
        tabcolor[node] = 'black' 
    elif listeownership[node] == '4' :
        tabcolor[node] = 'magenta'
    elif listeownership[node] == '5' :
        tabcolor[node] = 'darkorange' 
    elif listeownership[node] == '6' :
        tabcolor[node] = 'red'

        
pos = {0:(-13,7),
       1:(-3,9),
       2:(8,13),
       3:(-10,0),
       4:(-7,-1),
       5:(-4,2),
       6:(-3,1),
       7:(-0.5,3),
       8:(2.5,2),
       9:(2.8,6),
       10:(-11,-4),
       11:(-5,-11),
       12:(-5,-9),
       13:(-2,-10),
       14:(2,-13),
       15:(2,-9),
       16:(6,-13),
       17:(10,-14),
       18:(9,-7.5),
       19:(-11,-13.5),
       20:(-3.7,-13.6),
       21:(-13.7,-8),
       22:(-11,-8.5),
       23:(-8,-5.5),
       24:(-5.5,-6.5),
       25:(-8,-3),
       26:(-6.5,-3.2),
       27:(-5,-4),
       28:(-6,-2),
       29:(-3.2,-6),
       30:(-2,-7.5),
       31:(-1.9,-6.5),
       32:(-1,-9),
       33:(0.4,-9.5),
       34:(0.8,-11.2),
       35:(-0.3,-4.3),
       36:(0,-2.7),
       37:(1.5,-4),
       38:(3.5,-5),
       39:(4.5,-3),
       40:(1,-6.5),
       41:(-3.3,-1.8),
       42:(-2.2,-3.1),
       43:(-2,0.3),
       44:(-0.3,-0.3),
       45:(1,-1.5),
       46:(2,0),
       47:(3.6,-1.3),
       48:(5,2),
       49:(5,7),
       50:(9,6.5),
       51:(10,2),
       52:(7.5,-2.5),
       53:(14,-4),
       54:(16,-10),
       55:(15.5,-12.5),
       56:(10.5,-11.5),
       57:(11.5,-9.5),
       58:(8.5,-10.5),
       59:(3.8,-10.8),
       60:(6,-8.5),
       61:(7,-6.5),
       62:(3.5,-8),
       63:(3,-10),
       64:(-4.8,-1.5),
       65:(-4,-0.5),
       66:(-1.7,2.5),
       67:(0.8,3.7),
       68:(-0.8,6.2),
       69:(-7.6,4.6),
       70:(-6.7,4.4),
       71:(-7.4,1.6),
       72:(-6.5,1.5),
       73:(-8,0.6),
       74:(-6.7,-0.2),
}

nx.draw(G,pos,with_labels=False,node_size=100,node_color=tabcolor,edge_color="black")


img = mpimg.imread('diplomacy_map.webp')
plt.imshow(img, extent=[-18,18,-15,15])


ship = mpimg.imread('ship-removebg-preview.png')
arty = mpimg.imread('arty-removebg-preview.png')
industry = mpimg.imread('industrie-removebg-preview.png')

for node, (x,y) in pos.items():
    if listeindustry[node] == 'T' :
        """plt.text(x,y,"i", fontsize = 12, ha='left',va='bottom')"""
        plt.imshow(industry, extent=[x-1,x,y-1,y])
    if listegarrison[node] == 'A' :
        """plt.text(x,y,"a", fontsize = 12, ha='left',va='bottom')"""
        plt.imshow(arty, extent=[x,x+1.5,y-1.5,y-0.5])
    if listegarrison[node] == 'S' :
        """plt.text(x,y,"s", fontsize = 12, ha='right',va='bottom')"""
        plt.imshow(ship, extent=[x,x+1.5,y-1.5,y-0.5])

        
plt.show()


