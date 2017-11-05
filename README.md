## Drug-Trafficking-Organizations-Pattern-Recognition
In the 1990s New York City police investigated a cocaine crime ring by tapping phone
conversations between suspected members of a gang. We have in this dataset, 28 indi-
viduals who either placed and/or received a call from one of the others in the circle of
dealers. The left most column indicates the person who placed the call, and the col-
umn names indicate the person who was called. Hence, Lorena did not make any calls,
however she received 2 calls from just one person - Kay.

More about this dataset can be found in this paper: \Understanding the structure of
a drug tracking organization: a conversational analysis" by M Natarajan. The dataset
was downloaded from UCINET.


## Objectives:

1.	Manipulate the dataset to make it a tall table as shown below. Display this tall
table.

2.	Assume that each call is, just a connection. Compute the degree centrality of each
person. Display in a visual graph. The degree centrality of a node(person) i, can
be de_ned as the total number of nodes connected to node ni.

3.	Assume that each call is just a connection. Compute the betweenness centrality
of each person. Display in a visual graph. 

4.	Using the calling/called information, and ignoring the number of calls placed,
compute the indegree and outdegree centrality per person. Display in a visual
graph. Indegree and outdegree centrality are simply the count of inwards and
outwards directed connections to or from a node.

5.	Assume that a call is just an indication of a connection between two people and it
does not matter who placed or received the call. Can you visualize this network?

6.	Assume that you are interested in only outbound calls (a directed arrow showing
who called whom). Can you visualize this network?

7.	Assume that you are interested in only inbound calls (a directed arrow showing
who received the call from whom). Can you visualize this network?

8.	Compare the three networks you have visualized. What are your observations?
