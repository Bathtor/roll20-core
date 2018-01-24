package com.lkroll.roll20.core

trait FieldLike[T] {
  def editable(): Boolean;
  def ctx: RenderingContext;
  def attr: String;
  def qualifiedAttr: String = ctx.qualifier.map(q => s"${q}_$attr").getOrElse(attr);
  def name: String = s"attr_${qualifiedAttr}";
  def accessor: String = ctx.mapAccess(qualifiedAttr);
  def accessor(rowId: String): String = ctx.mapAccess(rowId, qualifiedAttr);
  def selector: String = ctx.mapSelect(qualifiedAttr);
  def initialValue: String;

  def reader: Readable[T];
  def read(s: String): Option[T] = reader.read(s);

  def expr: AutocalcExpression[T] = CoreImplicits.fieldToAuto(this);
  def arith()(implicit n: Numeric[T]): ArithmeticExpression[T] = CoreImplicits.autoToArith(this.expr);

  /*
   * Compare fields by attr
   */
  def canEqual(that: Any) = that.isInstanceOf[FieldLike[_]];
  override def hashCode(): Int = qualifiedAttr.hashCode();
  override def equals(that: Any): Boolean = canEqual(that) && (that.asInstanceOf[FieldLike[_]].qualifiedAttr == this.qualifiedAttr);
}
