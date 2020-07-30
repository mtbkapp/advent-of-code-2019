package advent_of_code_2019.day18;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;


public class Test {
  private static final String ex1 = "#########\n#b.A.@.a#\n#########";
  private static final String ex2 = "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################";
  private static final String ex3 = "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################";
  private static final String ex4 = "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################";
  private static final String ex5 = "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################";

  private static void is(boolean condition) throws Exception {
    if (!condition) {
      throw new Exception("test fail");
    }
  }

  private static void testParsing() throws Exception {
    Maze maze = Maze.fromString(ex1);

    is(2 == maze.keyCount);
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
          }
        }
      }
    }

    is((new Vector(5,1)).equals(maze.start));
  }

  private static void testNodeAdjacent() throws Exception {
    Maze maze = Maze.fromString(ex1);
    Node node = new Node(maze.start, new BitSet());
    List<Node> adj = node.adjacent(maze);

    is (2 == adj.size());
  }

  private static void testNodeHash() throws Excpeption {

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

  public static void test() {
    try {
      testVector();
      testParsing();
      testNodeAdjacent();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }
}
