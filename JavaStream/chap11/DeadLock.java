import java.util.ArrayList;
import java.util.List;

public class DeadLock {
    public static void main(String... args) {
        List<String> list1 = new ArrayList<>();
        List<String> list2 = new ArrayList<>();
        list1.add("list1-1");
        list2.add("list2-1");

        new Thread(new ResourceLock("ThreadA", list1, list2)).start();
        new Thread(new ResourceLock("ThreadB", list2, list1)).start();
    }
}
