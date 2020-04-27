package advent_of_code_2019;


public class day_16 {
  // Shared fns
  
  private static byte[] parse(String input) {
    byte[] out = new byte[input.length()];
    int i = 0;

    for(String digit : input.split("")) {
      out[i] = Byte.valueOf(digit);
      i++;
    }

    return out;
  }

  private static String render(byte[] xs) {
    StringBuffer buff = new StringBuffer();
    for(byte x : xs) {
      buff.append(x);
    }
    return buff.toString();
  }

  private static byte[] take(byte[] xs, int offset, int len) {
    byte[] out = new byte[len];
    System.arraycopy(xs, offset, out, 0, len);
    return out;
  }


  // Part 1

  private static byte buildValue(byte[] xs, int row) {
    final byte[] base = new byte[]{0, 1, 0, -1};
    long sum = 0;
    int baseIdx = 1;
    int repeats = row + 1;

    for(int i = row; i < xs.length; i++) {
      if (repeats == 0) {
        repeats = row + 1;
        baseIdx = (baseIdx + 1) % base.length;
      }

      sum += xs[i] * base[baseIdx];
      repeats--;
    }

    return (byte) (Math.abs(sum) % 10);
  }

  private static byte[] part1(byte[] in, int times) {
    byte[] out = new byte[in.length];
    byte[] temp;

    while(true) {
      for(int row = 0; row < in.length; row++) {
        out[row] = buildValue(in, row);
      }

      if (times == 1) break; 
      
      temp = in;
      in = out;
      out = temp;
      times--;
    }

    return take(out, 0, 8);
  }


  // Part 2

  private static int offset(byte[] xs) {
    int m = 1000000;
    int offset = 0;

    for(int i = 0; i < 7; i++) {
      offset += xs[i] * m;
      m = m / 10;
    }

    return offset;
  }

  private static byte[] repeat(byte[] xs, int times) {
    byte[] out = new byte[xs.length * times];
    for(int i = 0; i < xs.length * times; i++) {
      out[i] = xs[i % xs.length];
    }

    return out;
  }

  private static byte[] part2(byte[] xs, int times) {
    int off = offset(xs);
    byte[] rs = repeat(xs, 10000);

    for(int t = 0; t < times; t++) {
      for(int i = rs.length - 2; i >= off; i--) {
        rs[i] = (byte)((rs[i] + rs[i + 1]) % 10);
      }
    }

    return take(rs, off, 8);
  }

  public static void main(String[] args) {
    String puzzleInput = "59793513516782374825915243993822865203688298721919339628274587775705006728427921751430533510981343323758576985437451867752936052153192753660463974146842169169504066730474876587016668826124639010922391218906707376662919204980583671961374243713362170277231101686574078221791965458164785925384486127508173239563372833776841606271237694768938831709136453354321708319835083666223956618272981294631469954624760620412170069396383335680428214399523030064601263676270903213996956414287336234682903859823675958155009987384202594409175930384736760416642456784909043049471828143167853096088824339425988907292558707480725410676823614387254696304038713756368483311";
    System.out.println("Part 1: " + render(part1(parse(puzzleInput), 100)));
    System.out.println("Part 2: " + render(part2(parse(puzzleInput), 100)));
  }
}



