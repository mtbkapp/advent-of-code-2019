package advent_of_code_2019.day18;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Objects;

public class Node {
  private final Vector pos;
  private final BitSet keys;

  public Node(Vector pos, BitSet keys) {
    this.pos = pos;
    this.keys = keys;
  }

  public List<Node> adjacent(Maze maze) {
    List<Node> adj = new ArrayList<>();

    for(Vector v : pos.adjacent()) {
      Cell cell = maze.getCell(v);
      if (cell != null) {
        if (cell.isDoor() && haveKey(cell)) {
          adj.add(new Node(v, keys));
        } else if (cell.isKey()) {
          BitSet newKeys = (BitSet) keys.clone();
          newKeys.set(keyIndex(cell));
          adj.add(new Node(v, newKeys));
        } else if (cell.isSpace()) {
          adj.add(new Node(v, keys));
        }
      }
    }

    return adj;
  }

  public int keyIndex(Cell cell) {
    return Character.toLowerCase(cell.symbol) - 'a';
  }

  public boolean haveKey(Cell cell) {
    return keys.get(keyIndex(cell));
  }


  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Node) {
      Node rhs = (Node) obj;
      return pos.equals(rhs.pos) && keys.equals(rhs.keys);
    }


    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(pos.x, pos.y, keys);
  }
}
