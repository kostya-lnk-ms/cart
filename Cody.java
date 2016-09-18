import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.*;

/**
 * Created by kostya on 20/06/2016.
 */
public class Cody {

    private int hello;


    static class Heap<T extends Comparable<T>> {

        private final ArrayList<T> _input;

        public Heap(List<T> input) {
            _input = new ArrayList<T>(input);
            makeHeap();
        }

        private int left(int i) {
            return (i + 1) * 2 <= size() ? (i +1 ) * 2 - 1: size();
        }

        private int right(int i) {
            return (i + 1) * 2 < size() ? (i + 1) * 2 : size();
        }

        private void maxHeapify(int idx) {
            int l = left(idx);
            int r = right(idx);
            int largest =
             (l < size() && _input.get(l).compareTo(_input.get(idx)) > 0) ? l : idx;
            largest = (r < size() && _input.get(r).compareTo(_input.get(largest) ) > 0 ) ? r : largest;
            if (idx != largest) {
                T elem = _input.get(largest);
                _input.set(largest, _input.get(idx));
                _input.set(idx, elem);
                maxHeapify(largest);
            }
        }

        private void makeHeap() {
            for (int i = size() / 2 - 1; i >= 0; --i) {
                maxHeapify(i);
            }
        }

        final public int size() { return  _input.size(); }

        T max() { return size() > 0 ? _input.get(0) : null; }

        T popMax() {
            T m = max();
            _input.set(0, _input.get(size() - 1));
            _input.remove(_input.get(size() - 1));
            maxHeapify(0);
            return m;
        }

    }

    static <T> void swap(int i1, int i2, List<T> inp)  {
        T elem = inp.get(i1);
        inp.set(i1, inp.get(i2));
        inp.set(i2, elem);
    }

    static private <T extends Comparable<T>> int partition(int l, int r, List<T> inp) {
        int movePos = l;
        T pivot = inp.get(r);
        for (int j = l; j < r; ++j) {
            if ( inp.get(j).compareTo( pivot ) < 0 ) {
                if (movePos != j) {
                    swap(movePos, j, inp);
                }
                ++movePos;
            }
        }
        swap(movePos, r, inp);
        return movePos;
    }

    static private <T extends Comparable<T>> void quickSort(int l, int r, List<T> inp) {
        if ( l != r) {
            int p = partition(l, r, inp);
            if (p-1 > l ) quickSort(l, p - 1, inp);
            if (p+1 < r) quickSort(p + 1, r, inp);
        }
    }

    static <T extends Comparable<T>> void quickSort(List<T> inp) {

        quickSort(0, inp.size() - 1, inp);
    }


    String wrap(String src, int boundary) {

        StringBuilder sb = new StringBuilder(src.length() + src.length() / boundary);
        int lastPos = 0;
        for (int pos = 0; pos < src.length(); ++pos) {
            if (src.charAt(pos) == ' ' || pos == src.length() - 1) {
                if (pos - lastPos > boundary && pos != src.length() - 1) {
                    sb.append('\n');
                }
                sb.append(src.substring(lastPos, pos + 1));
                lastPos = pos + 1;
            }
        }
        return sb.toString();
    }


    public static void main(String[] args) {

        List<Integer> l = Arrays.asList(0, 0, -3, 5, 10);
        Heap<Integer> h = new Heap<>(l);
        while (h.size() > 0) {
            System.out.print(String.format("%d\n", h.popMax()));
        }

        quickSort(l);

        Executor ex = Executors.newCachedThreadPool();

        FutureTask<Integer> ft = new FutureTask<Integer>(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return 23;
            }
        });

        ex.execute( ft );

        try {
            Integer i = ft.get();
        } catch (InterruptedException e) {}
        catch (ExecutionException e) {}

        Cody c = new Cody();
        for (String s : Arrays.asList("", " ", "   ", " a", "a ", " a a  b c ")) {
            String r = c.wrap(s, 80);
            assert s.equals(r);
        }
        for (String s : Arrays.asList("1234567890 321", "1 1234567890")) {
            String r = c.wrap(s, 10);
            assert s.equals( s.replace(" ", "\n ") );
        }


        if (false)
        try {
            FileOutputStream f = new FileOutputStream("c:/temp/e");
            HashMap<Integer, String> m = new HashMap<>();
            ObjectOutputStream os = new ObjectOutputStream(f);
            m.put(1, "hello");
            m.put(333, "privet");
            os.writeObject(m);
            os.flush();
            os.close();
        }
            catch ( IOException e )
            {
                throw new RuntimeException(e);
            }

 //
    }

}
