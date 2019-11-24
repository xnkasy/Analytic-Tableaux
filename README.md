# Analytic-Tableaux

Analytic Tableaux is Trees built according to some rules, in simple terms. A Node in a tableau is labelled with a proposition and the truth value we wish to give it, i.e., as prop * bool. 

During the development of a tableau, nodes are introduced and examined.  When a node is examined, it is marked as examined and its analysis may result in:

1. Closing the paths on which it occurs because a contradiction has been discovered
2. Adding nodes and/or branches to each path on which this node lies, depending on the kind of node. 

A tableau is developed by selecting some unexamined node on a path which is not already closed due to a contradiction, and then analysing it.  Let p be the proposition and b be the boolean value in the label of the node being analysed:

  1. if p is T and b is true, or if p is F and b is false, then the node is marked examined and no extensions are made to the paths on which this node occurs
  2. if p is T and b is false, or if p is F and b is true, then the node is marked examined, and the paths on which this node occurs are considered closed  since they contain a contradiction.
  3. if another node with p and the negation of b is already on the path leading to this node, the current node is marked examined and the path considered closed as it contains a contradiction. [You can create a back-pointer to that earlier contradicting node]
  4. if p is L(s), then the propositional letter s is assigned the truth value b
  5. if p is Not(p1) and b is true [respectively false], then the node is marked examined and each open (non-closed) path on which it lies is extended with a node marked (p1, false) [respectively  (p1, true)]
  6. if the node is an α-type node, then it is marked examined, and each open (non-closed) path on which it lies is extended with two nodes α_1 and α_2, one below the other. 
  7. if the node is an β-type node, then it is marked examined, and each open (non-closed) path on which it lies is extended with two branches: on the first branch the node β_1 is placed, and on the second branch β_2 is placed.
  8. if p is of the form Iff(p1, p2) and b is false, then  it is marked examined, and each open (non-closed) path on which it lies is extended with two branches: on the first branch the nodes (p1, false) and (p2, true) are placed one below the other, and on the second branch the nodes (p1, true) and (p2, false) are placed one below the other.  If p is of the form Iff(p1, p2) and b is true, then  it is marked examined, and each open (non-closed) path on which it lies is extended with two branches: on the first branch the nodes (p1, true) and (p2, true) are placed one below the other, and on the second branch the nodes (p1, false) and (p2, false) are placed one below the other.  

A tableau is called "fully developed" if it has no unexamined nodes on any open path. The tableaux is represented by the data structure of the type "tree" defined as: node * bool * bool * tree * tree. The first two bools are for the questions: "Whether the node has been examined?" And "Whether any contradiction has been found?" respectively. 

## Using the code

The program can be compiled and run on the Ocaml terminal. The main functions of the package are as follows:

1. makeAna n gamma 

    => Inputs

      (i) n: Node
      
      (ii) gamma: A store for the current variable assignments. To be passed as [] ONLY

    => Outputs: A fully built Analytic Tableaux

2. contrad_path root gamma

    => Inputs

      (i) root: The input tree, partially built tableaux
      
      (ii) gamma: A store for the current variable assignments. To be passed as [] ONLY

    => Outputs: A node which has a contradiction in it. Marks it closed

3. valid_tableau root gamma

    => Inputs

      (i) root: The input tree, partially or fully built tableaux
      
      (ii) gamma: A store for the current variable assignments. To be passed as [] ONLY

    => Outputs: True if the given tree is valid as per the rules
    
4. select_node root

    => Inputs: A partially built tableaux

    => Outputs: An unexamined Node
    
5. step_develop node root

    => Inputs: 
    
      (i) node: The node under consideration which is not yet examined
      
      (ii) root: A partially built tableaux

    => Output: The fully completed subtree corresponding to that node as the root
    
6. 



