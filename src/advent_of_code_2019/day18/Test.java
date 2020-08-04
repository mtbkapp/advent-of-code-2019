package advent_of_code_2019.day18;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.PriorityQueue;
import java.util.TreeMap;


public class Test {
  public static final String ex1 = "#########\n#b.A.@.a#\n#########";
  public static final String ex2 = "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################";
  public static final String ex3 = "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################";
  public static final String ex4 = "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################";
  public static final String ex5 = "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################";
  public static final String ex6 = "###############\n#d.ABC.#.....a#\n######@#@######\n###############\n######@#@######\n#b.....#.....c#\n###############";
  public static final String ex7 = "#############\n#DcBa.#.GhKl#\n#.###@#@#I###\n#e#d#####j#k#\n###C#@#@###J#\n#fEbA.#.FgHi#\n#############";
  public static final String ex8 = "#############\n#g#f.D#..h#l#\n#F###e#E###.#\n#dCba@#@BcIJ#\n#############\n#nK.L@#@G...#\n#M###N#H###.#\n#o#m..#i#jk.#\n#############";

  private static void is(boolean condition) throws Exception {
    if (!condition) {
      throw new Exception("test fail");
    }
  }

  public static void prn(Object... args) {
    for(Object a : args) {
      System.out.print(a);
      System.out.print(" ");
    }

    System.out.println();
  }

  private static void testParsing() throws Exception {
    Maze maze = Maze.fromString(ex1);

    is(2 == maze.getKeyCount());
    is(null != maze.getCell(new Vector(0, 0)));
    is(null == maze.getCell(new Vector(-1, 0)));
    is(null == maze.getCell(new Vector(0, -1)));
    is(null == maze.getCell(new Vector(-1, -1)));
    is(null == maze.getCell(new Vector(10000, 10000)));
    is(null == maze.getCell(new Vector(10000, 0)));
    is(3 == maze.getHeight());
    is(9 == maze.getWidth());
    is(null == maze.getCell(new Vector(9, 0)));
    is(null == maze.getCell(new Vector(0, 3)));

    for(int y = 0; y < maze.getHeight(); y++) {
      for(int x = 0; x < maze.getWidth(); x++) {
        Cell c = maze.getCell(new Vector(x, y));
        is(c != null);
        if (y == 0 || y == 2) {
          is(Cell.CellType.WALL == c.type);
        } else {
          switch (x) { 
            case 1:
              is(Cell.CellType.KEY == c.type);
              is('b' == c.symbol);
              is(c.isKey());
              break;
            case 2:
              is(Cell.CellType.SPACE == c.type);
              is(c.isSpace());
              break;
            case 3:
              is(Cell.CellType.DOOR == c.type);
              is('A' == c.symbol);
              is(c.isDoor());
              break;
            case 4:
              is(Cell.CellType.SPACE == c.type);
              is(c.isSpace());
              break;
            case 5:
              is(Cell.CellType.START == c.type);
              is(c.isSpace());
              break;
            case 6:
              is(Cell.CellType.SPACE == c.type);
              is(c.isSpace());
              break;
            case 7:
              is(Cell.CellType.KEY == c.type);
              is('a' == c.symbol);
              is(c.isKey());
              break;
            default:
              is(Cell.CellType.WALL == c.type);
              break;
          }
        }
      }
    }

    is((new Vector(5,1)).equals(maze.firstStart()));
  }

  private static void testParsePart2() throws Exception {
    Maze maze = Maze.fromString(ex6);

    List<Vector> starts = maze.allStarts();
    is(4 == starts.size());

    HashSet<Vector> expected = new HashSet<>();
    expected.add(new Vector(6, 2));
    expected.add(new Vector(8, 2));
    expected.add(new Vector(6, 4));
    expected.add(new Vector(8, 4));

    for(Vector s : starts) {
      is(expected.contains(s));
    }
  }

  private static void testNode2Adjacent() throws Exception {
    Maze maze = Maze.fromString(ex6);
    Node2 n = Node2.startFromMaze(maze);

    List<Node> adj = n.adjacent(maze);
    is(4 == adj.size());

    Vector[] startRobots = n.getRobots();
    // robot 1 moves up, all others stay
    Vector[] robots = ((Node2) adj.get(0)).getRobots();
    is((new Vector(6,1)).equals(robots[0]));
    is(startRobots[1].equals(robots[1]));
    is(startRobots[2].equals(robots[2]));
    is(startRobots[3].equals(robots[3]));
    is(0 == adj.get(0).keyCount());
    
    // robot 2 moves up, all others stay
    robots = ((Node2) adj.get(1)).getRobots();
    is(startRobots[0].equals(robots[0]));
    is((new Vector(8,1)).equals(robots[1]));
    is(startRobots[2].equals(robots[2]));
    is(startRobots[3].equals(robots[3]));
    is(0 == adj.get(1).keyCount());
    
    // robot 3 moves down, all others stay
    robots = ((Node2) adj.get(2)).getRobots();
    is(startRobots[0].equals(robots[0]));
    is(startRobots[1].equals(robots[1]));
    is((new Vector(6,5)).equals(robots[2]));
    is(startRobots[3].equals(robots[3]));
    is(0 == adj.get(2).keyCount());

    // robot 4 moves down, all others stay
    robots = ((Node2) adj.get(3)).getRobots();
    is(startRobots[0].equals(robots[0]));
    is(startRobots[1].equals(robots[1]));
    is(startRobots[2].equals(robots[2]));
    is((new Vector(8,5)).equals(robots[3]));
    is(0 == adj.get(3).keyCount());


    // by door C, but no keys
    startRobots = new Vector[]{new Vector(6,1), new Vector(8,1), new Vector(6,4), new Vector(8,4)};
    Node2 start = new Node2(startRobots, KeySet.empty());
    adj = start.adjacent(maze);
    is(5 == adj.size());
    List<HashSet<Vector>> nextRobots = new ArrayList<>();
    nextRobots.add(new HashSet<>());
    nextRobots.add(new HashSet<>());
    nextRobots.add(new HashSet<>());
    nextRobots.add(new HashSet<>());

    for (Node adjNode : adj) {
      Node2 n2 = (Node2) adjNode;
      is(0 == n2.keyCount());
      int i = 0;
      for(Vector v : n2.getRobots()) {
        nextRobots.get(i).add(v);
        i++;
      }
    }

    HashSet<Vector> expected = new HashSet<>();
    expected.add(new Vector(6, 2));
    expected.add(new Vector(6, 1)); // robot stays put in most nodes
    is(expected.equals(nextRobots.get(0)));

    expected.clear();
    expected.add(new Vector(8,1));
    expected.add(new Vector(9,1));
    expected.add(new Vector(8,2));
    is(expected.equals(nextRobots.get(1)));

    expected.clear();
    expected.add(new Vector(6,4));
    expected.add(new Vector(6,5));
    is(expected.equals(nextRobots.get(2)));

    expected.clear();
    expected.add(new Vector(8,4));
    expected.add(new Vector(8,5));
    is(expected.equals(nextRobots.get(3)));
  
    // by door C, with key c 
    startRobots = new Vector[]{new Vector(6,1), new Vector(8,1), new Vector(6,4), new Vector(8,4)};
    start = new Node2(startRobots, KeySet.withKeys('c'));
    adj = start.adjacent(maze);
    is(6 == adj.size());

    for(HashSet<Vector> nr : nextRobots) {
      nr.clear();
    }
    for (Node adjNode : adj) {
      Node2 n2 = (Node2) adjNode;
      is(1 == n2.keyCount());
      int i = 0;
      for(Vector v : n2.getRobots()) {
        nextRobots.get(i).add(v);
        i++;
      }
    }

    expected.clear();
    expected.add(new Vector(6, 2));
    expected.add(new Vector(6, 1));
    expected.add(new Vector(5, 1));
    is(expected.equals(nextRobots.get(0)));

    expected.clear();
    expected.add(new Vector(8,1));
    expected.add(new Vector(9,1));
    expected.add(new Vector(8,2));
    is(expected.equals(nextRobots.get(1)));

    expected.clear();
    expected.add(new Vector(6,4));
    expected.add(new Vector(6,5));
    is(expected.equals(nextRobots.get(2)));

    expected.clear();
    expected.add(new Vector(8,4));
    expected.add(new Vector(8,5));
    is(expected.equals(nextRobots.get(3)));
  
 
    // key c already collected, by door C, also by key a
    startRobots = new Vector[]{new Vector(6,1), new Vector(12,1), new Vector(6,4), new Vector(8,4)};
    start = new Node2(startRobots, KeySet.withKeys('c'));
    adj = start.adjacent(maze);
    is(6 == adj.size());
    
    for(HashSet<Vector> nr : nextRobots) {
      nr.clear();
    }
    for (Node adjNode : adj) {
      Node2 n2 = (Node2) adjNode;
      boolean keyLoc = false;
      int i = 0;
      for(Vector v : n2.getRobots()) {
        nextRobots.get(i).add(v);
        i++;

        if(v.equals(new Vector(13,1))) {
          keyLoc = true;
        } 
      }

      if (keyLoc) {
        is(KeySet.withKeys('c','a').equals(n2.getKeys()));
      } else {
        is(KeySet.withKeys('c').equals(n2.getKeys()));
      }
    }

    expected.clear();
    expected.add(new Vector(6, 2));
    expected.add(new Vector(6, 1));
    expected.add(new Vector(5, 1));
    is(expected.equals(nextRobots.get(0)));

    expected.clear();
    expected.add(new Vector(11,1));
    expected.add(new Vector(12,1));
    expected.add(new Vector(13,1));
    is(expected.equals(nextRobots.get(1)));

    expected.clear();
    expected.add(new Vector(6,4));
    expected.add(new Vector(6,5));
    is(expected.equals(nextRobots.get(2)));

    expected.clear();
    expected.add(new Vector(8,4));
    expected.add(new Vector(8,5));
    is(expected.equals(nextRobots.get(3)));
  }

  private static void testNodeAdjacent() throws Exception {
    final Maze maze = Maze.fromString(ex1);
    final Node1 n0 = new Node1(maze.firstStart(), KeySet.empty()); 
    final List<Node> adj0 = n0.adjacent(maze);

    // adjacent to maze.start = (5,1) 
    is (2 == adj0.size());
    HashSet<Vector> expectedVectors = new HashSet<>();
    expectedVectors.add(new Vector(4,1));
    expectedVectors.add(new Vector(6,1));
    for (Node nx : adj0) {
      Node1 n = (Node1) nx;
      is(0 == n.keyCount());
      is(expectedVectors.contains(n.getPos()));
    }

    // adjacent to (4,1) without key a
    final Node1 n1 = new Node1(new Vector(4,1), KeySet.empty()); 
    final List<Node> adj1 = n1.adjacent(maze); 
    is(1 == adj1.size());
    is(0 == adj1.get(0).keyCount());
    is((new Vector(5, 1)).equals(((Node1) adj1.get(0)).getPos()));

    // adjacent to (4,1) with key a
    KeySet keys2 = KeySet.withKeys('a');
    final Node1 n2 = new Node1(new Vector(4,1), keys2);
    final List<Node> adj2 = n2.adjacent(maze); 
    is (2 == adj2.size());
    HashSet<Vector> ev2 = new HashSet<>();
    ev2.add(new Vector(3,1));
    ev2.add(new Vector(5,1));
    for (Node nx : adj2) {
      Node1 n = (Node1) nx;
      is(1 == n.keyCount());
      is(ev2.contains(n.getPos()));
    }
 
    // adjacent to (6,1) 
    final Node1 n3 = new Node1(new Vector(6, 1), KeySet.empty()); 
    final List<Node> adj3 = n3.adjacent(maze);
    HashMap<Vector, Integer> ev3 = new HashMap<>();
    ev3.put(new Vector(5, 1), 0);
    ev3.put(new Vector(7, 1), 1);
    for (Node nx : adj3) {
      Node1 n = (Node1) nx;
      is(ev3.containsKey(n.getPos()));
      is(ev3.get(n.getPos()).equals(n.keyCount()));
    }
  }

  private static void testNodeHash() throws Exception {
    // same data same hash
    Node1 node0 = new Node1(new Vector(2, 0), KeySet.withKeys('a', 'b'));
    Node1 node1 = new Node1(new Vector(2, 0), KeySet.withKeys('a', 'b'));
    is(node0.hashCode() == node1.hashCode());

    HashSet<Node1> set = new HashSet<>();
    set.add(node0);
    set.add(node1);

    is(set.size() == 1);

    // different vectors
    Node1 node2 = new Node1(new Vector(2, 1), KeySet.withKeys('a', 'b'));
    is(node2.hashCode() != node0.hashCode());
    
    Node1 node3 = new Node1(new Vector(3, 0), KeySet.withKeys('a', 'b'));
    is(node3.hashCode() != node0.hashCode());
    
    // different keyset
    Node1 node4 = new Node1(new Vector(2, 0), KeySet.withKeys('a','c','d'));
    is(node4.hashCode() != node0.hashCode());
  }

  private static void testVector() throws Exception {
    // equals works
    Vector v = new Vector(4,5); 
    is(4 == v.x);
    is(5 == v.y);
    is(v.equals(v));
    is(v.equals(new Vector(4, 5)));
    is(!v.equals(new Vector(0, 0)));

    // adjacent
    is(4 == v.adjacent().length);

    // hashes probably correctly
    HashSet<Vector> set = new HashSet<>();
    set.add(new Vector(0, 0));
    set.add(new Vector(0, 1));
    set.add(new Vector(0, 1));
    is(2 == set.size());
  }

  private static void testUpdatablePriorityQueue() throws Exception {
    KeySet keys = KeySet.withKeys('a', 'e');
    Node1 n0 = new Node1(new Vector(0, 1), keys);
    Node1 n0a = new Node1(new Vector(0, 1), keys);
    Node1 n1 = new Node1(new Vector(2, 2), keys);
    UpdatablePriorityQueue q = new UpdatablePriorityQueue();

    // priority queue works and can get dist
    is(q.isEmpty());
    is(Integer.MAX_VALUE == q.getDist(n0));

    q.enqueue(n0, 3);
    q.enqueue(n1, 2);

    is(!q.isEmpty());
    is((new NodeDist(n1, 2)).equals(q.peek()));

    is(!q.isEmpty());
    is((new NodeDist(n1, 2)).equals(q.poll()));
    is((new NodeDist(n0, 3)).equals(q.poll()));

    is(q.isEmpty());


    // can update dist
    q.enqueue(n0, 3);
    q.enqueue(n1, 2);
    is(3 == q.getDist(n0));
    is(2 == q.getDist(n1));
    is((new NodeDist(n1, 2)).equals(q.peek()));

    q.updateDist(n1, 55);
    is(55 == q.getDist(n1));
    is((new NodeDist(n0, 3)).equals(q.peek()));
  }

  public static void testNode2() throws Exception {
    KeySet keys = KeySet.withKeys('a', 'b', 'c');
    Node2 n0 = new Node2(new Vector[]{new Vector(2, 1), new Vector(3, 1), new Vector(5, 1), new Vector(6, 1)}, keys);
    Node2 n0a = new Node2(new Vector[]{new Vector(2, 1), new Vector(3, 1), new Vector(5, 1), new Vector(6, 1)}, keys);

    is(n0.equals(n0a));
    is(n0a.equals(n0));
    is(n0a.hashCode() == n0.hashCode());

    // different robot
    Node2 n1 = new Node2(new Vector[]{new Vector(1, 1), new Vector(3, 1), new Vector(5, 1), new Vector(6, 1)}, keys);
    is(!(n0.equals(n1)));
    is(n0.hashCode() != n1.hashCode());
    
    n1 = new Node2(new Vector[]{new Vector(2, 1), new Vector(0, 1), new Vector(5, 1), new Vector(6, 1)}, keys);
    is(!(n0.equals(n1)));
    is(n0.hashCode() != n1.hashCode());
    
    n1 = new Node2(new Vector[]{new Vector(2, 1), new Vector(3, 1), new Vector(0, 1), new Vector(6, 1)}, keys);
    is(!(n0.equals(n1)));
    is(n0.hashCode() != n1.hashCode());

    n1 = new Node2(new Vector[]{new Vector(2, 1), new Vector(3, 1), new Vector(5, 1), new Vector(0, 1)}, keys);
    is(!(n0.equals(n1)));
    is(n0.hashCode() != n1.hashCode());

    // different keyset
    n1 = new Node2(new Vector[]{new Vector(2, 1), new Vector(3, 1), new Vector(5, 1), new Vector(6, 1)}, KeySet.withKeys('a'));
    is(!(n0.equals(n1)));
    is(n0.hashCode() != n1.hashCode());

    // adjacent nodes
  }

  public static void prnTime(String label, Runnable fn) {
    long n0 = System.nanoTime();
    fn.run();
    prn(label, (System.nanoTime() - n0) / 1000000.0);
  } 

  public static void testPart1() throws Exception {
    is(8 == Maze.fromString(ex1).minDistToAllKeysSingleStart());
    is(86 == Maze.fromString(ex2).minDistToAllKeysSingleStart());
    is(132 == Maze.fromString(ex3).minDistToAllKeysSingleStart());
    is(136 == Maze.fromString(ex4).minDistToAllKeysSingleStart());
    is(81 == Maze.fromString(ex5).minDistToAllKeysSingleStart());

    prnTime("ex1", () -> {
      prn(8 == Maze.fromString(ex1).minDistToAllKeysSingleStart());
    });
    prnTime("ex2", () -> {
      prn(86 == Maze.fromString(ex2).minDistToAllKeysSingleStart());
    });
    prnTime("ex3", () -> {
      prn(132 == Maze.fromString(ex3).minDistToAllKeysSingleStart());
    });
    prnTime("ex4", () -> {
      prn(136 == Maze.fromString(ex4).minDistToAllKeysSingleStart());
    });
    prnTime("ex5", () -> {
      prn(81 == Maze.fromString(ex5).minDistToAllKeysSingleStart());
    });
  }

  public static void testPart2() throws Exception {
    is(24 == Maze.fromString(ex6).minDistFromAllStarts());
    is(32 == Maze.fromString(ex7).minDistFromAllStarts());

    prnTime("ex6", () -> {
      prn(Maze.fromString(ex6).minDistFromAllStarts());
    });
    prnTime("ex7", () -> {
      prn(Maze.fromString(ex7).minDistFromAllStarts());
    });
  }

  public static void test() {
    try {
      testParsing();
      testNodeAdjacent();
      testNode2Adjacent();
      testNodeHash();
      testVector();
      testUpdatablePriorityQueue();
      testPart1();
      testNode2();
      testParsePart2();
      testPart2();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }
}
