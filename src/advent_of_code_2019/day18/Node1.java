package advent_of_code_2019.day18;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


public class Node1 implements Node {
  private final Vector pos;
  private final KeySet keys;

  public Node1(Vector pos, KeySet keys) {
    this.pos = pos;
    this.keys = keys;
  }

  public List<Node> adjacent(Maze maze) {
    List<Node> adj = new ArrayList<>();

    for(Vector v : pos.adjacent()) {
      Cell cell = maze.getCell(v);
      if (cell != null) {
        if (cell.isDoor() && haveKey(cell)) {
          adj.add(new Node1(v, keys));
        } else if (cell.isKey()) {
          adj.add(new Node1(v, keys.addKey(cell.symbol)));
        } else if (cell.isSpace()) {
          adj.add(new Node1(v, keys));
        }
      }
    }

    return adj;
  }

  public boolean haveKey(Cell cell) {
    return keys.contains(cell.symbol); 
  }

  public KeySet getKeys() {
    return keys;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Node1) {
      Node1 rhs = (Node1) obj;
      return pos.equals(rhs.pos) && keys.equals(rhs.keys);
    }


    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(pos.x, pos.y, keys);
  }

  public int keyCount() {
    return keys.size();
  }

  public Vector getPos() {
    return pos;
  }

  @Override
  public String toString() {
    return "Node1(" + pos + ", " + keys + ")";
  }
}
