package sri.tools.graphqltosjs

import io.scalajs.nodejs.fs.Fs
import io.scalajs.nodejs.global

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

object Generator {

  val gqlTypes = Map("String" -> "String",
                     "Float" -> "Double",
                     "Int" -> "Int",
                     "Boolean" -> "Boolean",
                     "DateTime" -> "String",
                     "ID" -> "String")

  var ast: GraphQlParsed = null;

  def getScalaType(in: GraphQLDefinitionFieldType, isRequired: Boolean = false): String = {

    if (in.kind == GraphQLDefinitionFieldTypeKind.LIST_TYPE) {
      if (isRequired) s"js.Array[${getScalaType(in.`type`, true)}]"
      else s"js.UndefOr[js.Array[${getScalaType(in.`type`, true)}]]"
    } else if (in.kind == GraphQLDefinitionFieldTypeKind.NON_NULL_TYPE) {
      getScalaType(in.`type`, true)
    } else if (in.kind == GraphQLDefinitionFieldTypeKind.NAMED_TYPE) {
      val t = in.name.value
      if (isRequired) gqlTypes.getOrElse(t, t)
      else s"js.UndefOr[${gqlTypes.getOrElse(t, t)}]"
    } else getScalaType(in.`type`, false)
  }

  def convertGraphFieldToScalaField(in: GraphQLDefinitionField) = {
    val name = getScalaName(in.name.value)
    //    println(s"type dude : ${JSON.stringify(in.`type`)}")
    val tpe = getScalaType(in.`type`)
    if (in.arguments.exists(_.nonEmpty)) {
      val args = in.arguments.get.map(f => {
        val tpe = getScalaType(f)
        ScalaField(name = getScalaName(f.name.value),
                   tpe = tpe,
                   isRequired = !tpe.startsWith("js.UndefOr["))
      })
      ScalaField(name = name, tpe = tpe, arguments = args)
    } else ScalaField(name, tpe)
  }

  def getInterfaceFieldNamesByInterfaceName(in: String): Set[String] = {

    ast.definitions
      .find(d => d.kind == GraphQLDefinitionKind.InterfaceTypeDefinition && d.name.value == in)
      .map(d => d.fields.map(_.name.value).toSet)
      .getOrElse(Set())
  }

  def generateArgumentType(objectName: String, field: ScalaField): String = {

    val name = s"${field.name.capitalize}${objectName}Argument"

    val fields = field.arguments

    val result =
      s"""
         |
       |
       |trait $name extends js.Object  {
         |  ${fields
           .map(sf =>
             s"val ${sf.name} :${sf.tpe} ${if (sf.tpe.contains("UndefOr[")) s" = js.undefined"
             else ""}")
           .mkString("\n")}
         |}
         |
       |object $name {
       |
       | def apply(${fields
           .map(sf =>
             s"${sf.name} :${if (sf.tpe.startsWith("js.UndefOr[")) sf.tpe.replace("js.UndefOr[", "OptionalParam[")
             else sf.tpe} ${if (sf.tpe.startsWith("js.UndefOr[")) s" = OptDefault"
             else ""}")
           .mkString(",\n")}):$name = {
         |
         | val op = FunctionObjectMacro()
         |   op.asInstanceOf[$name]
         | }
         |
       |}
         |
     """.stripMargin
    result
  }

  def convertObjectDefinitionToScala(in: GraphQLDefinition) = {
    //    dom.window.console.log("converting type", in.name.value)
    val traitName = in.name.value
    val interfaces = in.interfaces.map(gi => gi.name.value).toList
    val interfaceFields =
      in.interfaces.toSet.flatMap((i: GraphQLDefinitionInterface) =>
        getInterfaceFieldNamesByInterfaceName(i.name.value))
    val ext =
      if (interfaces.length == 0) "js.Object"
      else if (interfaces.length == 1) interfaces.head
      else s"${interfaces.head} with ${interfaces.tail.mkString("with")}"
    val fields = in.fields.map(convertGraphFieldToScalaField)

    val args = fields
      .filter(f => f.arguments.nonEmpty)
      .map(f => generateArgumentType(traitName, f))
      .mkString("\n")
    s"""
       |
       | $args
       |@js.native
       |trait $traitName extends $ext  {
       |  ${fields
         .map(sf => {
           if (sf.arguments.nonEmpty && (traitName == "Query" || traitName == "Mutation" || traitName == "Subscription")) {

             val argName = s"${sf.name.capitalize}${traitName}Argument"
             val args = sf.arguments
               .map(a =>
                 s"${a.name}:${a.tpe}${if (a.tpe.startsWith("js.UndefOr[")) " =  js.undefined"
                 else ""}")
               .mkString(",")
             s"${if (interfaceFields.contains(sf.name)) "override" else ""} def ${sf.name}(arg:$argName,info: scalajsgraphql.GraphQLResolveInfo | String = ???,options:js.Object= ???) :js.Promise[${sf.tpe}] = js.native"
           } else {
             s"${if (interfaceFields.contains(sf.name)) "override" else ""} val ${sf.name} :${sf.tpe} = js.native"
           }
         }) //Bloody  hack to get rid of id
         .mkString("\n")}
       |}
     """.stripMargin
  }

  def convertInterfaceDefinitionToScala(in: GraphQLDefinition) = {
//    dom.window.console.log("converting type", in.name.value)
    val traitName = in.name.value
    val fields = in.fields.map(convertGraphFieldToScalaField)
    s"""
       |
       |@js.native
       |trait $traitName extends js.Object  {
       |  ${fields
         .map(sf => s"val ${sf.name} :${sf.tpe} = js.native")
         .mkString("\n")}
       |}
     """.stripMargin
  }

  def convertInputObjectTypeDefinitionToScala(in: GraphQLDefinition) = {
//    dom.window.console.log("converting type", in.name.value)
    val traitName = in.name.value
    val fields = in.fields.map(convertGraphFieldToScalaField)
    s"""
       |
       |
       |trait $traitName extends js.Object  {
       |  ${fields
         .map(sf =>
           s"val ${sf.name} :${sf.tpe} ${if (sf.tpe.contains("UndefOr[")) s" = js.undefined"
           else ""}")
         .mkString("\n")}
       |}
       |
       |object $traitName {
       |
       | def apply(${fields
         .map(sf =>
           s"${sf.name} :${sf.tpe.replace("js.UndefOr[", "OptionalParam[")} ${if (sf.tpe.contains("UndefOr[")) s" = OptDefault"
           else ""}")
         .mkString(",\n")}):$traitName = {
       |
       |   val p = FunctionObjectMacro()
       |   p.asInstanceOf[$traitName]
       | }
       |
       |}
       |
     """.stripMargin
  }

  def convertEnumTypeDefinitionToScala(in: GraphQLDefinition) = {
//    dom.window.console.log("converting type", in.name.value)
    val traitName = in.name.value
    val values = in.values.map(_.name.value)
    s"""
       |
       |@js.native
       |trait $traitName extends js.Object
       |
       |object $traitName {
       |  ${values
         .map(v => s"""@inline def $v = "$v".asInstanceOf[$traitName] """)
         .mkString("\n")}
       |}
       |
     """.stripMargin
  }

  def getArg(args: Seq[String], name: String, shortName: String = ""): js.UndefOr[String] = {
    val index = args.zipWithIndex
      .find {
        case (f, i) => if (shortName.nonEmpty) (f == name || f == shortName) else (f == name)
      }
      .map(_._2)
      .getOrElse(-1)
    if (index == -1 || args.length == index + 1) js.undefined else args(index + 1)
  }

  def geExistsType() = {
    val queryType = ast.definitions.find(d =>
      d.kind == GraphQLDefinitionKind.ObjectTypeDefinition && d.name.value == "Query")
    queryType
      .map(d => {
        val fields = d.fields
          .map(f => convertGraphFieldToScalaField(f))
          .filter(sf => {
            sf.tpe.startsWith("js.Array[") || sf.tpe.startsWith("js.UndefOr[js.Array[")
          })
          .map(sf => {
            val name =
              if (sf.tpe.startsWith("js.Array[")) sf.tpe.substring(sf.tpe.indexOf("[") + 1).init
              else {
                val x = sf.tpe.substring(sf.tpe.indexOf("[") + 1).init
                x.substring(x.indexOf("[") + 1).init
              }
            val whereName = sf.arguments
              .find(sf => sf.name == "where")
              .map(sf =>
                if (sf.tpe.startsWith("js.UndefOr[")) sf.tpe.substring(sf.tpe.indexOf("[") + 1).init
                else sf.tpe)
              .get
            s""" def $name(where:$whereName = ???) : js.Promise[Boolean] = js.native"""
          })

        fields.mkString("\n")
      })
      .getOrElse("")
  }

  def main(args: Array[String]): Unit = {
    val commandArgs = global.process.argv.toList

    val SCHEMA_PATH = getArg(commandArgs, "--schema", "--s").getOrElse("./data/schema.graphql")

    val OUTPUT_PATH = getArg(commandArgs, "--output", "--o").getOrElse(".")

    val RELAY_URL = getArg(commandArgs, "--relayUrl", "--u").getOrElse("")

    val in = Fs.readFileSync(SCHEMA_PATH, "utf8").toString()
    ast = GraphQL.parse(in)
    val tpes = ast.definitions
      .map(d => {
        d.kind match {
          case GraphQLDefinitionKind.ObjectTypeDefinition =>
            convertObjectDefinitionToScala(d)
          case GraphQLDefinitionKind.EnumTypeDefinition =>
            convertEnumTypeDefinitionToScala(d)
          case GraphQLDefinitionKind.InputObjectTypeDefinition =>
            convertInputObjectTypeDefinitionToScala(d)
          case GraphQLDefinitionKind.InterfaceTypeDefinition =>
            convertInterfaceDefinitionToScala(d)
          case _ => ""
        }
      })
      .mkString("\n")

    val packagePath = OUTPUT_PATH
      .substring(OUTPUT_PATH.indexOf("src/main/scala/") + 15)
      .split("/")
      .mkString(".")

    val out =
      s"""
         |package ${packagePath}
         
         |import scala.scalajs.js
         |import scala.scalajs.js.annotation.{JSImport}
         |import scala.scalajs.js.|
         |import scalajsgraphql.prismabinding.PrismaOptions
         |import scalajsplus.{
         |  OptDefault,
         |  OptionalParam
         |}
         |import scalajsplus.macros.FunctionObjectMacro
         |import scalajsgraphql.GraphQLResolveInfo
         |
         | /** this file is automatically generated on ${new js.Date()
           .toISOString()}
         |
         |  don't modify this file directly */
         | $tpes
         |
         | @js.native
         | trait Exists extends js.Object {
         |   ${geExistsType()}
         | }
         |
         |@js.native
          trait Prisma extends js.Object {

           val query:Query= js.native
           val mutation:Mutation = js.native
           val subscription:Subscription = js.native
           val exists:Exists = js.native
           def request[T](query:String,variables:js.Object= ???):js.Promise[T] = js.native
           def delegate(operation:String,fieldName: String,args: js.Object,infoOrQuery:GraphQLResolveInfo | String = ???,options: js.Object = ???):js.Promise[js.Any] = js.native
           def delegateSubscription(fieldName: String,args: js.Object,infoOrQuery:GraphQLResolveInfo | String = ???,options: js.Object = ???):js.Promise[js.Any] = js.native
          }

          object Prisma {

            def apply(options:PrismaOptions) = new scalajsgraphql.prismabinding.JSPrisma(options).asInstanceOf[Prisma]
          }
         |
       """.stripMargin

    Fs.writeFileSync(s"$OUTPUT_PATH/Prisma.scala", out)

    global.console.log(s"Successfully generated models!.")

  }
}
