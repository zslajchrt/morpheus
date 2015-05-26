package org.morpheus;

/**
 * Created by zslajchrt on 14/01/15.
 */
public class MorphUtils {

    public static Object context(Object fragment) {
        Object context = MorphContext$.MODULE$.context(fragment);
        return context;
    }

    public static Object $super$(Object fragment) {
        Object sup = SuperContext$.MODULE$.$super$(fragment);
        return sup;
    }
}
