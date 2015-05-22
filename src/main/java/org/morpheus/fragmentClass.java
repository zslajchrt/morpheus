package org.morpheus;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 *
 * This annotation is used by the compiler plugin to gather extra information about the fragment used during
 * the macro expansion. It is placed on generated fragment classes.
 *
 * Created by zslajchrt on 28/01/15.
 *
 **/
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.CONSTRUCTOR})
public @interface fragmentClass {
    Class<?>[] deps();
    Class<?> config() default Object.class;
}
