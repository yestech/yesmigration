package org.yestech.migration

class MysqlTextColumnDefinition
  extends ColumnDefinition
{
  override
  val sql = "TEXT"
}

class MySQLDatabaseAdapter(override val schemaNameOpt: Option[String]) extends DatabaseAdapter(schemaNameOpt) {

  override def removeIndexSql(schema_name_opt: Option[String], table_name: String, index_name: String): String = ""

  override val addingForeignKeyConstraintCreatesIndex: Boolean = false

  override val unquotedNameConverter =  CasePreservingUnquotedNameConverter

  override def quoteTableName(tableName: String): String = tableName
  override def quoteColumnName(columnName: String): String = columnName

  override def lockTableSql(table_name: String) = {
//    "select 1"
    "LOCK TABLES " + table_name + " WRITE "
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
  def columnDefinitionFactory (column_type: SqlType, character_set_opt: Option[CharacterSet]): ColumnDefinition = {

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
}
