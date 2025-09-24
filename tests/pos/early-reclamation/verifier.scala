import scala.annotation.lastUse

class Obj{}



object Verif:

    private def use(o: Obj): Unit =
        println(o)


    def test1 =
        val o = Obj()

        use(o)
        val y = o
        use(o)
        use(o:  @lastUse)

    def test2 =
        val o = Obj()

        use(o)
        if "hi".length() == 2 then
            use(o:  @lastUse)
        else
            if "hello".length == 3 then
                use(o)
            else
                use(o:   @lastUse)

    def test3 =
        val o = Obj()

        use(o)

        val x = Some(1)

        x match
            case None => 
                use(o: @lastUse)
            case Some(x) => 
                use(o: @lastUse)

    def test4 = 
        val o = Obj()

    def test5 =
        val o  = Obj()

        f()

        use(o)

        def f() = 
            use(o)

    
        
        