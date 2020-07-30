package advent_of_code_2019;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.PriorityQueue;



public class day_18 {
  private static final String ex1 = "#########\n#b.A.@.a#\n#########";
  private static final String ex2 = "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################";
  private static final String ex3 = "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################";
  private static final String ex4 = "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################";
  private static final String ex5 = "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################";

  public static void main(String[] args) {
    day_18 x = new day_18();
    x.test();
    x.testDijkstraStep();
    System.out.println(x.findAllKeys(ex1));
    System.out.println(x.findAllKeys(ex2));
  }

  public int findAllKeys(String maze) {
    return findAllKeys(parse(maze));
  }

  public void test() {
    testQ();
  }

  public void testQ() {
    // ensure PriorityQueue of State objects works as expected
    PriorityQueue<State> q = new PriorityQueue<>();
    byte width = 0;
    byte height = 0;
    byte keyCount = 2;
    Cell[][] dummyMaze = new Cell[height][width];
    BitSet s0Keys = new BitSet(keyCount);

    State s0 = new State(dummyMaze, new Vector(0, 0), s0Keys, keyCount, width, height, 3);
    State s1 = new State(dummyMaze, new Vector(0, 1), s0Keys, keyCount, width, height, 4);
    State s1b = new State(dummyMaze, new Vector(0, 1), s0Keys, keyCount, width, height, 2);

    // state with min distance is always first

    q.add(s1);
    q.add(s0);

    is(q.poll() == s0);
    is(q.poll() == s1);
    is(q.isEmpty());

    // can update distance by removing and adding new object
    q.add(s0);
    q.add(s1);

    is(q.peek() == s0);
    q.remove(s1);
    q.add(s1b);
    is(q.peek() == s1b);
  }

  public void is(boolean condition) {
    if (!condition) {
      throw new IllegalStateException("condition failed");
    }
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
  int findAllKeys(State start) {
    int dist = -1;

    HashMap<State, Integer> dists = new HashMap<>();
    HashSet<State> visited = new HashSet<>();
    PriorityQueue<State> q = new PriorityQueue<State>(1000000);
    q.add(start);

    while(!q.isEmpty()) {
      int stepResult = dijkstraStep(q, visited, dists);
      if (stepResult != -1) {
        return stepResult;
      }
    }

    return dist;
  }

  int dijkstraStep(PriorityQueue<State> q, HashSet<State> visited, HashMap<State, Integer> dists) {
    State u = q.poll();
    visited.add(u);

    if (u.hasAllKeys()) {
      return u.dist;
    }

    for(State v : u.adjacent()) {
      if(!visited.contains(v)) {
        int alt = u.dist + 1; 
        int currDist = dists.getOrDefault(v, Integer.MAX_VALUE);

        if (currDist == Integer.MAX_VALUE) {
          // v not in q or dists
          q.add(v);
          dists.put(v, alt);
        } else if (alt < currDist) {
          // found a shorter path to v
          q.remove(v);
          q.add(v.withDist(alt));
          dists.put(v, alt);
        } else {
          // found v again but via a longer path
        }
      }
    }

    return -1;
  }

  void testDijkstraStep() {
    HashSet<State> visited = new HashSet<>();

  }

  State parse(String input) {
    String[] lines = input.split("\n");
    byte height = (byte) lines.length;
    byte width = (byte) lines[0].length();
    Vector start = null;
    byte keyCount = 0;
    Cell[][] maze = new Cell[height][width];

    for(int y = 0; y < height; y++) {
      String line = lines[y];
      for(int x = 0; x < width; x++) {
        char c = line.charAt(x);
        Cell cell = null;

        if (c == '#') {
          cell = new Cell(CellType.WALL);
        } else if (c == '.') {
          cell = new Cell(CellType.SPACE);
        } else if (c == '@') {
          cell = new Cell(CellType.START);
          start = new Vector(x, y);
        } else if (Character.isLowerCase(c)) {
          cell = new Cell(CellType.KEY, c);
          keyCount++;
        } else if (Character.isUpperCase(c)) {
          cell = new Cell(CellType.DOOR, c);
        } else {
          throw new IllegalStateException("Invalid cell: " + c);
        }

        maze[y][x] = cell;
      }
    }

    if (keyCount == 0) {
      throw new IllegalStateException("didn't find any keys!");
    }

    if (start == null) {
      throw new IllegalStateException("didn't find start cell");
    }

    BitSet keys = new BitSet(keyCount);
    return new State(maze, start, keys, keyCount, width, height, 0); 
  }

  public enum CellType {
    WALL,
    START,
    SPACE,
    DOOR,
    KEY
  }


  public class Cell {
    public final CellType type;
    public final char name; 

    Cell(CellType type) {
      this(type, '-');
    }

    Cell(CellType type, char name) {
      this.type = type;
      this.name = Character.toLowerCase(name);
    }

    boolean isKey() {
      return CellType.KEY == this.type;
    }

    boolean isDoor() {
      return CellType.DOOR == this.type;
    }

    boolean isSpace() {
      return CellType.START == this.type || CellType.SPACE == this.type;
    }
  }


  public class Vector {
    public final int x;
    public final int y;

    Vector(int x, int y) {
      this.x = x;
      this.y = y;
    }

    Vector move(Vector dir) {
      return new Vector(x + dir.x, y + dir.y);
    }

    boolean inBounds(int width, int height) {
      return x >= 0 && x < width && y >= 0 && y < height;
    }

    @Override
    public boolean equals(Object obj) {
      Vector rhs = (Vector) obj;
      return x == rhs.x && y == rhs.y;
    }
  }


  public class State implements Comparable {
    private final Vector[] dirs = new Vector[]{ new Vector(1,0), new Vector(-1, 0), new Vector(0,1), new Vector(0,-1) };
    public final Cell[][] maze;
    public final Vector pos;
    public final BitSet keys;
    public final byte keyCount;
    public final byte width;
    public final byte height;
    public final int dist;

    State(Cell[][] maze, Vector pos, BitSet keys, byte keyCount, byte width, byte height, int dist) {
      this.maze = maze;
      this.pos = pos;
      this.keys = keys;
      this.keyCount = keyCount;
      this.width = width;
      this.height = height;
      this.dist = dist;
    }

    State addKey(char key) {
      BitSet newKeys = (BitSet) keys.clone();
      newKeys.set(keyIndex(key));
      return new State(maze, pos, newKeys, keyCount, width, height, dist);
    }

    boolean hasAllKeys() {
      return keys.cardinality() == keyCount; 
    }

    boolean haveKey(char name) {
      return keys.get(keyIndex(name)); 
    }

    int keyIndex(char key) {
      return key - 'a';
    }

    ArrayList<State> adjacent() {
      ArrayList<State> adj = new ArrayList<>();
      for (Vector dir : dirs) {
        Vector newPos = pos.move(dir);
        if (newPos.inBounds(width, height)) {
          Cell cell = maze[newPos.y][newPos.x];
          if (cell.isSpace()) {
            adj.add(new State(maze, newPos, keys, keyCount, width, height, dist + 1));
          } else if (cell.isDoor() && haveKey(cell.name)) {
            adj.add(new State(maze, newPos, keys, keyCount, width, height, dist + 1));
          } else if (cell.isKey() && !haveKey(cell.name)) {
            // can move to cell and not pick up key?
            BitSet newKeys = (BitSet) keys.clone();
            newKeys.set(keyIndex(cell.name));
            adj.add(new State(maze, newPos, newKeys, keyCount, width, height, dist + 1));
          }
        }
      }

      return adj;
    }

    State withDist(int newDist) {
      return new State(maze, pos, keys, keyCount, width, height, newDist);
    }

    @Override
    public boolean equals(Object obj) {
      State rhs = (State) obj;
      return keys.equals(rhs.keys) && pos.equals(rhs.pos);
    }

    public int compareTo(Object rhs) {
      int x = this.dist;
      int y = ((State)rhs).dist;

      if (x < y) {
        return -1;
      } else if (y < x) {
        return 1;
      } 

      return 0;
    }
  }
}
