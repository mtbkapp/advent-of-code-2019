package advent_of_code_2019.day18;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;
import java.util.List;

public class Maze {
  private Cell[][] cells;
  private final List<Vector> starts;
  private final int keyCount;

  private Maze(Cell[][] cells, List<Vector> starts, int keyCount) {
    this.cells = cells;
    this.starts = starts;
    this.keyCount = keyCount;
  }

  public Cell getCell(int x, int y) {
    if (x >= 0 && x < getWidth() && y >= 0 && y < getHeight()) {
      return cells[y][x];
    }

    return null;
  }

  public Cell getCell(Vector pos) {
    return getCell(pos.x, pos.y);
  }

  public int getHeight() {
    return cells.length;
  }

  public int getWidth() {
    return cells[0].length;
  }

  public int getKeyCount() {
    return keyCount;
  }

  public Vector firstStart() {
    return starts.get(0);
  }

  public List<Vector> allStarts() {
    return starts;
  }

  @Override
  public String toString() {
    StringBuffer buff = new StringBuffer();

    for(Cell[] row : cells) {
      for(Cell cell : row) {
        buff.append(cell.symbol);
      }
      buff.append("\n");
    }

    return buff.toString(); 
  }

  public static Maze fromString(String input) {
    String[] lines = input.split("\n");
    int height = lines.length;
    int width = lines[0].length();
    int keyCount = 0;
    Cell[][] cells = new Cell[height][width];
    List<Vector> starts = new ArrayList<>(); 

    for(int y = 0; y < height; y++) {
      String line = lines[y];

      for(int x = 0; x < width; x++) {
        char c = line.charAt(x); 
        Cell cell = Cell.fromChar(c);
        cells[y][x] = cell;

        if (cell.type == Cell.CellType.START) {
          starts.add(new Vector(x, y));
        } else if (cell.type == Cell.CellType.KEY) {
          keyCount++;
        }
      }
    }

    return new Maze(cells, starts, keyCount); 
  }


//; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
//; 1  function Dijkstra(Graph, source):
//; 2
//; 3      create vertex set Q
//; 4
//; 5      for each vertex v in Graph:             
//; 6          dist[v] ← INFINITY                  
//; 7          prev[v] ← UNDEFINED                 
//; 8          add v to Q                      
//;10      dist[source] ← 0                        
//;11      
//;12      while Q is not empty:
//;13          u ← vertex in Q with min dist[u]    
//;14                                              
//;15          remove u from Q 
//;16          
//;17          for each neighbor v of u:           // only v that are still in Q
//;18              alt ← dist[u] + length(u, v)
//;19              if alt < dist[v]:               
//;20                  dist[v] ← alt 
//;21                  prev[v] ← u 
//;22
//;23      return dist[], prev[]


  public int minDistToAllKeysSingleStart() {
    return minDistToAllKeys(new Node1(firstStart(), KeySet.empty()));
  }

  public int minDistToAllKeys(Node start) {
    HashSet<Node> visited = new HashSet<>();
    UpdatablePriorityQueue q = new UpdatablePriorityQueue();
    q.enqueue(start, 0);
    
    while(!q.isEmpty()) {
      NodeDist uDist = q.poll();
      Node u = uDist.node;
      int dist = uDist.dist;
      visited.add(u);

      if (keyCount == u.keyCount()) {
        return dist;
      }

      for(Node v : u.adjacent(this)) {
        if(!visited.contains(v)) {
          int alt = dist + 1;
          q.addIfNotExists(v, alt);
          if (alt < q.getDist(v)) {
            q.updateDist(v, alt);
          }
        }
      }
    }

    return -1;
  }
}
