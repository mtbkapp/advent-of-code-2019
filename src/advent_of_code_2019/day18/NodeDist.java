package advent_of_code_2019.day18;

import java.util.Objects;

public class NodeDist implements Comparable<NodeDist> {
  public final Node node;
  public final int dist;

  public NodeDist(Node node, int dist) {
    this.node = node;
    this.dist = dist;
  }

  public int compareTo(NodeDist rhs) {
    return Integer.compare(dist, rhs.dist);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof NodeDist) {
      NodeDist rhs = (NodeDist) obj;
      return node.equals(rhs.node) && dist == rhs.dist;
    }

    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(node, dist);
  }

  @Override
  public String toString() {
    return "NodeDist(" + node.toString() + ", " + dist + ")";
  }
}
