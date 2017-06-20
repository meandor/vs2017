package mware_lib.communication;

import java.lang.reflect.InvocationTargetException;

public class ReflectionUtil {


    public static Object call(Object object, String method, Object... args) {
        Class<?>[] argTypes = new Class<?>[args.length];
        for(int i = 0;i<argTypes.length;i++){
            argTypes[i]=toClass(args[i]);
        }
        try {
            return object.getClass().getDeclaredMethod(method, argTypes).invoke(object, args);
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            e.printStackTrace();
        }
        return null;
    }

    private static Class<?> toClass(Object o){
        if(o.getClass()==Integer.class){
            return int.class;
        }else if(o.getClass()==Double.class){
            return double.class;
        }
        return o.getClass();
    }

}
