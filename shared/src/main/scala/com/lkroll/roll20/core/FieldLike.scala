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
  def nameMatcher: String => Boolean = ctx.mapMatcher(qualifiedAttr);
  def nameMatcherRow(rowId: String): String => Boolean = ctx.mapMatcher(rowId, qualifiedAttr);
  def initialValue: String;

  def reader: Readable[T];
  def read(s: String): Option[T] = reader.read(s);

  lazy val isMax: Boolean = attr.endsWith("_max");

  def expr(implicit labelFields: LabelFields): AutocalcExpression[T] = CoreImplicits.fieldToAutoMaybeLabel(this);
  def arith()(implicit n: Numeric[T], labelFields: LabelFields): ArithmeticExpression[T] = CoreImplicits.autoToArith(this.expr);

  /*
   * Compare fields by attr
   */
  def canEqual(that: Any) = that.isInstanceOf[FieldLike[_]];
  override def hashCode(): Int = qualifiedAttr.hashCode();
  override def equals(that: Any): Boolean = canEqual(that) && (that.asInstanceOf[FieldLike[_]].qualifiedAttr == this.qualifiedAttr);
}
