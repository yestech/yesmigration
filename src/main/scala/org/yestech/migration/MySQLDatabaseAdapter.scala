package org.yestech.migration

class MysqlTextColumnDefinition
  extends ColumnDefinition {
  override
  val sql = "TEXT"
}

/**
 * Specifies an Auto Increment Column
 */
case object AutoIncrement
  extends ColumnOption {
  override def toSql = {
    " AUTO_INCREMENT "
  }
}

/**
 * Engine type for a table
 */
case class Engine(name: String) extends TableOption {
  override def toSql = {
    " ENGINE=" + name + " "
  }
}

/**
 * Comment for a table
 */
case class Comment(comment: String) extends TableOption {
  override def toSql = {
    " COMMENT='" + comment + "' "
  }
}

class MySQLDatabaseAdapter(override val schemaNameOpt: Option[String]) extends DatabaseAdapter(schemaNameOpt) {

  override def removeIndexSql(schema_name_opt: Option[String], table_name: String, index_name: String): String = ""

  override val addingForeignKeyConstraintCreatesIndex: Boolean = false

  override val unquotedNameConverter = CasePreservingUnquotedNameConverter

  override def quoteTableName(tableName: String): String = tableName

  override def quoteColumnName(columnName: String): String = columnName

  override def lockTableSql(table_name: String) = {
    //    "select 1"
    "LOCK TABLE " + table_name + " WRITE "
  }

  override def unlockTableSql(table_name: String) = {
    //    "select 1"
    "UNLOCK TABLE "
  }

  protected def alterColumnSql(schema_name_opt: Option[String], column_definition: ColumnDefinition) = {
    new java.lang.StringBuilder(512)
      .append("ALTER TABLE ")
      .append(quoteTableName(schema_name_opt, column_definition.getTableName))
      .append(" ALTER COLUMN ")
      .append(quoteColumnName(column_definition.getColumnName))
      .append(" ")
      .append(column_definition.toSql)
      .toString
  }

  override
  def columnDefinitionFactory(column_type: SqlType, character_set_opt: Option[CharacterSet]): ColumnDefinition = {

    column_type match {
      case BigintType =>
        new DefaultBigintColumnDefinition
      case BlobType =>
        new MysqlTextColumnDefinition
      case BooleanType =>
        new DefaultBooleanColumnDefinition
      case CharType =>
        new DefaultCharColumnDefinition
      case DecimalType =>
        new DefaultDecimalColumnDefinition
      case IntegerType =>
        new DefaultIntegerColumnDefinition
      case TimestampType =>
        new DefaultTimestampColumnDefinition
      case SmallintType =>
        new DefaultSmallintColumnDefinition
      case VarbinaryType =>
        new PostgresqlByteaColumnDefinition
      case VarcharType =>
        new DefaultVarcharColumnDefinition
    }
  }

  override def generateSql(tableOptions: List[TableOption]): String = {
    val sql = new java.lang.StringBuilder(512)

    for (option <- tableOptions) {
      option match {
        case CharacterSet(Unicode) => sql.append(" CHARSET=utf8 ")
        case opt @ _ => sql.append(opt.toSql)
      }
    }
    sql.toString
  }
}
