val pageStyle = "background-color:#99CCFF; margin:10px;font-size:small;"
val centerStyle = "text-align:center;"
val headerStyle = "text-align:center;font-size:100%;color:maroon;"
val codeStyle = "font-size:90%;"

val Turtle = "t"
val Pictures = "p"
val ControlFlow = "cf"
val Abstraction = "a"

def navLinks() =
    <p style={ headerStyle }>
        <a href={ "http://localpage/" + Turtle }>Turtle</a> | <a href={ "http://localpage/" + Pictures }>Picture</a> <br/>
        <a href={ "http://localpage/" + ControlFlow }>Flow</a> | <a href={ "http://localpage/" + Abstraction }>Abstraction</a>
        <hr/>
        <br/>
    </p>

import scala.collection.mutable.LinkedHashMap

val tTemplates = LinkedHashMap(
    "clear()" -> "clear()",
    "invisible()" -> "invisible()",
    "cleari()" -> "cleari()",
    "setAnimationDelay()" -> "setAnimationDelay(100)",
    "forward(n)" -> "forward(50)",
    "right(a)" -> "right(90)",
    "left(a)" -> "left(90)",
    "penUp()" -> "penUp()",
    "penDown()" -> "penDown()",
    "setPenColor(c)" -> "setPenColor(blue)",
    "setFillColor(c)" -> "setFillColor(blue)",
    "setBackground(c)" -> "setBackground(yellow)",
    "setPenThickness(t)" -> "setPenThickness(4)"
)

val cfTemplates = LinkedHashMap(
    "repeat(n) {cmds}" -> """repeat (4) {
    forward(50)
    right()
}""",
    "if" -> """if (true) {
    setPenColor(blue)
}""",
    "if-else" -> """if (true) {
    setPenColor(blue)
}
else {
    setPenColor(green)
}""",
    "for (cmd)" -> """for (i <- 1 to 4) {
    println(i)
}"""
)

val aTemplates = LinkedHashMap(
    "val" -> "val x = 10",
    "def (cmd)" -> """def square(n: Int) {
    repeat (4) {
        forward(50)
        right()
    }
} """,
    "def (fn)" -> """def max(n1: Int, n2: Int) = 
        if (n1 > n2) n1 else n2"""
)

val pTemplates = LinkedHashMap(
    "PShapes.hline(len)" -> "PShapes.hline(50)",
    "PShapes.vline(len)" -> "PShapes.vline(50)",
    "PShapes.rect(w, h)" -> "PShapes.rect(50, 100)",
    "PShapes.ball(r)" -> "PShapes.ball(50)",
    "PShapes.text(s, n)" -> """PShapes.text("Hello", 18)""",
    "Picture" -> """Picture {
    
}""",
    "PictureT" -> """PictureT { t =>
    import t._
    forward(100)
    
}""",
    "HPics(pics)" -> "HPics(PShapes.hline(50), PShapes.vline(50))",
    "VPics(pics)" -> "VPics(PShapes.vline(50), PShapes.hline(50))",
    "GPics(pics)" -> "GPics(PShapes.hline(50), PShapes.vline(50))",
    "draw(pics)" -> "draw(PShapes.hline(50), PShapes.vline(50))"
)

val instructions = Map(
    "t" -> tTemplates.keys.toIndexedSeq,
    "cf" -> cfTemplates.keys.toIndexedSeq,
    "a" -> aTemplates.keys.toIndexedSeq,
    "p" -> pTemplates.keys.toIndexedSeq
)

val templates = Map(
    "t" -> tTemplates,
    "cf" -> cfTemplates,
    "a" -> aTemplates,
    "p" -> pTemplates
)

def runLink(category: String, n: Int) = s"http://runhandler/$category/$n"
def code(category: String, n: Int) = {
    <div style="background-color:CCFFFF;margin-top:3px"> 
        <pre><code><a href={ runLink(category, n) } style="text-decoration: none;font-size:x-small;"> { instructions(category)(n) }</a></code></pre>
    </div>
}

def pageFor(cat: String) = Page(
    name = cat,
    body =
        <body style={ pageStyle }>
        { navLinks }
        { for (i <- 0 until instructions(cat).length) yield (code(cat, i)) }
        </body>
)

val story = Story(
    pageFor(Turtle),
    pageFor(ControlFlow),
    pageFor(Abstraction),
    pageFor(Pictures)
)
stClear()
stSetStorytellerWidth(50)

import javax.swing._
val helpFrame = new JFrame
helpFrame.setUndecorated(true)
helpFrame.setBounds(300, 100, 500, 300)
val helpPane = new JEditorPane
helpPane.setBackground(Color(255, 255, 51))
helpPane.setContentType("text/html")
val helpScroller = new JScrollPane(helpPane)
helpScroller.setBorder(BorderFactory.createEmptyBorder())
helpFrame.getContentPane.add(helpScroller)

def insertCode(cat: String, idx: Int) {
    stInsertCode(templates(cat)(instructions(cat)(idx)))
}

stAddLinkHandler(Turtle, story) { idx: Int => insertCode(Turtle, idx) }
stAddLinkHandler(ControlFlow, story) { idx: Int => insertCode(ControlFlow, idx) }
stAddLinkHandler(Pictures, story) { idx: Int => insertCode(Pictures, idx) }
stAddLinkHandler(Abstraction, story) { idx: Int => insertCode(Abstraction, idx) }

def keyFor(cat: String, n: Int) = {
    val instr = instructions(cat)(n)
    if (instr.contains("."))
        instr.dropWhile(_ != '.').drop(1).takeWhile(c => c != '(' && c != '-').trim
    else
        instr.takeWhile(c => c != '(' && c != '-').trim
}
def showHelp(cat: String, idx: Int) {
    helpPane.setText(s"""<body style="background-color:#ffff99;margin:10px;">
        ${stHelpFor(keyFor(cat, idx))}
        </body>
        """
    )
    helpPane.setCaretPosition(0)
    helpFrame.setVisible(true)
}

stAddLinkEnterHandler(Turtle, story) { idx: Int => showHelp(Turtle, idx) }
stAddLinkEnterHandler(ControlFlow, story) { idx: Int => showHelp(ControlFlow, idx) }
stAddLinkEnterHandler(Pictures, story) { idx: Int => showHelp(Pictures, idx) }
stAddLinkEnterHandler(Abstraction, story) { idx: Int => showHelp(Abstraction, idx) }

stOnStoryStop(story) {
    helpFrame.setVisible(false)
    helpFrame.dispose()
}
stPlayStory(story)
runInBackground { stSetScript("") }
