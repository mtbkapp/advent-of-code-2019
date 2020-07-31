package advent_of_code_2019.day18;

import java.util.BitSet;

public class KeySet {
  private BitSet keys;

  private KeySet(BitSet keys) {
    this.keys = keys;
  }

  public KeySet addKey(char k) {
    BitSet newKeys = (BitSet) keys.clone();
    newKeys.set(keyIndex(k));

    return new KeySet(newKeys);
  }

  public boolean contains(char k) {
    return keys.get(keyIndex(k));
  }

  public int size() {
    return keys.cardinality();
  }

  public static KeySet empty() {
    return new KeySet(new BitSet());
  }

  public static KeySet withKeys(char... keys) {
    BitSet set = new BitSet();
    for(char k : keys) {
      set.set(keyIndex(k));
    }

    return new KeySet(set);
  }

  private static int keyIndex(char k) {
    return Character.toLowerCase(k) - 'a';
  }

  @Override
  public String toString() {
    return keys.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof KeySet) {
      KeySet rhs = (KeySet) obj;
      return keys.equals(rhs.keys);
    }

    return false;
  }

  @Override
  public int hashCode() {
    return keys.hashCode();
  }
}
