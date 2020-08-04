package advent_of_code_2019.day18;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


public class Node2 implements Node {
  private final Vector[] robots;
  private final KeySet keys;

  public Node2(Vector[] robots, KeySet keys) {
    this.robots = robots;
    this.keys = keys;
  }

  public List<Node> adjacent(Maze maze) {
    List<Node> adj = new ArrayList<>();
    for(int i = 0; i < robots.length; i++) {
      Vector u = robots[i];
      for(Vector v : u.adjacent()) {
        // robot i wants to move to v
        Cell cell = maze.getCell(v);  
        if((cell.isDoor() && keys.contains(cell.symbol)) || cell.isSpace()) {
          // move robot i to v
          adj.add(new Node2(moveRobot(i, v), keys));
        } else if (cell.isKey()) {
          // move robot i to v and pick up key
          adj.add(new Node2(moveRobot(i, v), keys.addKey(cell.symbol)));
        }
      }
    }

    return adj;
  }

  private Vector[] moveRobot(int idx, Vector v) {
    Vector[] next = copyRobots(); 
    next[idx] = v;

    return next;
  }

  private Vector[] copyRobots() {
    Vector[] next = new Vector[robots.length];
    System.arraycopy(robots, 0, next, 0, robots.length);

    return next;
  }

  public Vector[] getRobots() {
    return copyRobots();
  }

  public int keyCount() {
    return keys.size();
  }

  public KeySet getKeys() {
    return keys; 
  }

  public static Node2 startFromMaze(Maze maze) {
    Vector[] robots = maze.allStarts().toArray(new Vector[]{});
    return new Node2(robots, KeySet.empty());
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Node2) {
      Node2 rhs = (Node2) obj;
      return robots[0].equals(rhs.robots[0]) &&
        robots[1].equals(rhs.robots[1]) &&
        robots[2].equals(rhs.robots[2]) &&
        robots[3].equals(rhs.robots[3]) &&
        keys.equals(rhs.keys);
    }

    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(robots[0], robots[1], robots[2], robots[3], keys);
  }

  @Override
  public String toString() {
    return "Node2(" + robots[0] + ", " + robots[1] + ", " + robots[2] + ", " + robots[3] + ", " + keys + ")";
  }
}
