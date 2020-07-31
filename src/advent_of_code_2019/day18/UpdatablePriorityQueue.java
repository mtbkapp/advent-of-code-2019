package advent_of_code_2019.day18;

import java.util.HashMap;
import java.util.PriorityQueue;

public class UpdatablePriorityQueue {
  private HashMap<Node, Integer> dists; 
  private PriorityQueue<NodeDist> q;

  public UpdatablePriorityQueue() {
    this.dists = new HashMap<>();
    this.q = new PriorityQueue<>();
  }

  public NodeDist peek() {
    return q.peek();
  }

  public NodeDist poll() {
    return q.poll();
  } 

  public int getDist(Node n) {
    return dists.getOrDefault(n, Integer.MAX_VALUE);
  }

  public boolean isEmpty() {
    return q.isEmpty();
  }

  public void updateDist(Node n, int dist) {
    int oldDist = getDist(n);
    dists.put(n, dist);
    q.remove(new NodeDist(n, oldDist));
    q.add(new NodeDist(n, dist));
  }

  public void enqueue(Node n, int dist) {
    dists.put(n, dist);
    q.add(new NodeDist(n, dist));
  }

  public void addIfNotExists(Node n, int dist) {
    if (!dists.containsKey(n)) {
      enqueue(n, dist);
    }
  }
}
