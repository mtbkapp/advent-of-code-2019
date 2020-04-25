package advent_of_code_2019;


public class Day16 {

  class RepeatingSeq {
    private final byte[] base = new byte[]{0, 1, 0, -1};
    private int repeatReset;
    private int repeats;
    private int baseIdx = 0;

    RepeatingSeq(int repeats) {
      this.repeatReset = repeats;
      this.repeats = repeats;
    }

    byte next() {
      repeats--;
      if (repeats == 0) {
        repeats = repeatReset;
        baseIdx = (baseIdx + 1) % base.length;
      }

      return base[baseIdx];
    }

    byte[] take(int n) {
      byte[] out = new byte[n];

      for(int i = 0; i < n; i++) {
        out[i] = next();
      }

      return out;
    }
  }

  class RepeatingMatrix {
    private byte[][] matrix;

    RepeatingMatrix(int size) {
      matrix = new byte[size][size];

      for(int i = 0; i < size; i++) {
        RepeatingSeq repSeq = new RepeatingSeq(i + 1);
        matrix[i] = repSeq.take(size);
      }
    }

    private byte multAdd(byte[] a, byte[] b) {
      long sum = 0;
      for(int i = 0; i < a.length; i++) {
        sum += (a[i] * b[i]);
      }

      return (byte) (Math.abs(sum) % 10);
    }

    byte[] multVec(byte[] vec) {
      byte[] out = new byte[vec.length];

      for(int i = 0; i < vec.length; i++) {
        out[i] = multAdd(vec, matrix[i]);
      }


      return out;
    }
  }

  /*
  private long multAdd(long[] a, long[] b) {
    long out = 0;
    for(int i = 0; i < a.length; i++) {
      out += (a[i] * b[i]);
    }

    return Math.abs(out) % 10;
  }

  private long[] phase(long[] input) {
    long[] output = new long[input.length];

    for(int i = 0; i < input.length; i++) {
      long[] repeatingSeq = (new RepeatingSeq(i + 1)).take(input.length);
      output[i] = multAdd(repeatingSeq, input);
    }

    return output;
  }

  private long[] phaseTimes(long[] input, int times) {
    for(int i = 0; i < times; i++) {
      input = phase(input);
    }

    return input;
  }
  */

  private String render(byte[] input) {
    StringBuffer buff = new StringBuffer();

    for(int i = 0; i < input.length; i++) {
      buff.append(input[i]);
    }


    return buff.toString();
  }


  private byte[] take(int n, byte[] input) {
    byte[] out = new byte[n];

    for(int i = 0; i < n; i++) {
      out[i] = input[i];
    }

    return out;
  }

  private byte[] repeatTimes(byte[] input, int times) {
    byte[] out = new byte[input.length * times];
    for (int i = 0; i < (input.length * times); i++) {
      out[i] = input[i % input.length];
    }

    return out;
  }

  private int getOffset(byte[] input) {
    final int offsetLen = 7;
    StringBuffer buff = new StringBuffer();
    // TODO use math instead
    for(int i = 0; i < offsetLen; i++) {
      buff.append(input[i]);
    }

    return Integer.valueOf(buff.toString());
  }

  private byte[] drop(byte[] input, int n) {
    byte[] out = new byte[input.length - n];
    System.arraycopy(input, n, out, 0, out.length);
    return out;
  }

  private byte[] getPart2Input(byte[] input) {
    return drop(repeatTimes(input, 10000), getOffset(input));
  }

  private byte[] parseInput(String input) {
    byte[] out = new byte[input.length()];
    int i = 0;

    for(String digit : input.split("")) {
      out[i] = Byte.valueOf(digit);
      i++;
    }

    return out;
  }

  // part 1 - 49254779
  private static final String puzzleInput = "59793513516782374825915243993822865203688298721919339628274587775705006728427921751430533510981343323758576985437451867752936052153192753660463974146842169169504066730474876587016668826124639010922391218906707376662919204980583671961374243713362170277231101686574078221791965458164785925384486127508173239563372833776841606271237694768938831709136453354321708319835083666223956618272981294631469954624760620412170069396383335680428214399523030064601263676270903213996956414287336234682903859823675958155009987384202594409175930384736760416642456784909043049471828143167853096088824339425988907292558707480725410676823614387254696304038713756368483311";

  private static final String ex1 = "03036732577212944063491565474664";
  private static final String ex2 = "02935109699940807407585447034323";
  private static final String ex3 = "03081770884921959731165446850517";

  public void go() {
    System.out.println("start");
    long start = System.currentTimeMillis();
    //long[] s = phaseTimes(repeatTimes(parseInput(puzzleInput), 10000), 1); 
    //long[] s = repeatTimes(parseInput(puzzleInput), 10000); 
    //RepeatingSeq rs = new RepeatingSeq(100);
    //long[] row = rs.take(6500000);
    
    //RepeatingSeq rs = new RepeatingSeq(100);
    //long[] row = rs.take(520649);

    //System.out.println(puzzleInput.length());
    //System.out.println(getOffset(parseInput(puzzleInput)));

    //long[] row = getPart2Input(parseInput(puzzleInput));
    //long[] row = phaseTimes(), 1); 
    
    byte[] input = getPart2Input(parseInput(ex1));
    RepeatingMatrix repMat = new RepeatingMatrix(input.length);
    for(int i = 0; i < 100; i++) {
      System.out.println("phase " + i);
      input = repMat.multVec(input);
    }
    //RepeatingMatrix repMat = new RepeatingMatrix(520649);

    System.out.println("Duration: " + (System.currentTimeMillis() - start));
    System.out.println(render(take(8, input)));
  }

  /*
  static long[] parseInput(String input) {
    long[] out = new long[input.length()];
    int i = 0;

    for(String digit : input.split("")) {
      out[i] = Long.valueOf(digit);
      i++;
    }

    return out;
  }
  */

  private static final byte[] base = new byte[]{0, 1, 0, -1};

  private static byte buildValue(int row, byte[] input) {
    long sum = 0;
    int baseIdx = 1;
    int repeats = row + 1;

    for (int i = row; i < input.length; i++) {
      if (repeats == 0) {
        repeats = row + 1;
        baseIdx = (baseIdx + 1) % base.length;
      }

      sum += input[i] * base[baseIdx];
      repeats--;
    }

    return ((byte) (Math.abs(sum) % 10));
  }


  public static void main(String[] args) {
    byte[] input = new byte[]{1,2,3,4,5,6,7,8};
    System.out.println(buildValue(0, input));
    System.out.println(buildValue(1, input));
    System.out.println(buildValue(2, input));
    System.out.println(buildValue(3, input));
    System.out.println(buildValue(4, input));
    System.out.println(buildValue(5, input));
    System.out.println(buildValue(6, input));
    System.out.println(buildValue(7, input));


    //(new Day16()).go();
  }
}
