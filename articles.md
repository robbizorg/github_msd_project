# Notes on Various Academic Articles

## Coding Together at Scale: GitHub as a Collaborative Social Network

Analyzes social ties and repository-mediated collaobration patterns, and remarkably low level of repocity of the social connections--look at how distance influences collaboration

"First in-depth quantitative analysis of Github" (2)

Power-law-like distribution of contributors/watchers for projects (aka long-tail)

Very active users necessarily do not have large numbers of followers

Very low repocity of social ties--remarkably different from other papers

Users tend to interact with people they are close to, repos with low num of collaborators tend to be concentrated in particular geographic locations

Brandes et al 2009: Collaboration network analysis of Wikipedia

Github Archive: https://www.githubarchive.org/ -- Possible dataset to use instead of API

#### Structural Analysis

G_f: Followers Graph, directed representation of users' following relations

G_c: Collaborators graph, repository nodes linked to collaborators nodes (bipartite graph)

G_c': Projected Collaborators Graph, graph obtained by projecting the collaborators graph onto the set of users. Users who collaobrate in at least one repository are connected to each other

G_s: stargazers graph, bipartite graph of users and the repos they watch (in watch events)

G_n: Contributors graph, derived from content of push events, including authorship info of pushed commits

Following people is 'high cost', results in a lot of updates from them (4)

Low Reciprocity of G_f: only 9.6% of pairs os users follow each other--Twitter has 22.1%, 84%; "rockstar programmers" might explain this, have high in-degrees and low out-degrees; but authors believe different nature of github might play a role. Github is mostly not an entertainment platform, productivity concerns might cause developers to not follow others since following is 'high cost'

Rich-club Phenomenon: Tendency of high degree nodes to form tightly interconnected communities. There's a 'rich-club coefficient' defined in the paper, might want to use this!

Network seems to be slightly disassortative, where nodes with a small number of links tend to share edges with low degree nodes

Finding on rich-club: Figure 5 indicates that popular developers tend to share links with lower degree nodes rather than being tightly interconnected among them

wrt G_c': STRONG RICH-CLUB phenomenon, each group of collaborators forms a clique (5). G_c' has a high clustering coefficient 

Social Interactions in G_f and G_c are remarkably different, hence the different structures

Note (regarding their dataset): One active repository out of four is exclusively authored by a single individual

Official Collaboration Status rare: only 9.61% of repositories have them (5, double check to make sure I read that right)

The rest of the paper explores the relationship between geography and following/collaboration

Long-range links have a higher cost, harder to maintain
