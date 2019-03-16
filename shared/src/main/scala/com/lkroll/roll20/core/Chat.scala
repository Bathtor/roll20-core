/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Lars Kroll <bathtor@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.lkroll.roll20.core

sealed trait ChatCommand extends Renderable {
  def apiType: Option[ChatType.ChatType];
  def message(content: String): ChatOutMessage = CommandMessage(this, content);
}

object ChatType extends Enumeration {
  type ChatType = Value;

  val general, rollresult, gmrollresult, emote, whisper, desc, api = Value;
}

sealed trait ChatOutMessage extends Renderable;

final case class SimpleMessage(message: String) extends ChatOutMessage {
  override def render: String = message;
}
final case class CommandMessage(cmd: ChatCommand, content: String) extends ChatOutMessage {
  override def render: String = s"${cmd.render} $content";
}

object Chat {

  def fromString(s: String): ChatCommand = {
    s match {
      case " "                         => Default
      case "/w gm "                    => GM
      case "/em "                      => Emote
      case "/ooc "                     => OutOfCharacter
      case "/desc "                    => Description
      case _ if s.startsWith("/w ")    => Whisper(extractTarget("/w ", s))
      case _ if s.startsWith("/as ")   => SpeakAs(extractTarget("/as ", s))
      case _ if s.startsWith("/emas ") => EmoteAs(extractTarget("/emas ", s))
      case _ if s.startsWith("/fx ") => {
        val parts = s.split(' ');
        if (parts.length == 3) {
          val ctParts = parts(1).split('-');
          assert(ctParts.length == 2);
          SpecialEffects(ctParts(0), ctParts(1), parts(2))
        } else if (parts.length == 4) {
          val ctParts = parts(1).split('-');
          assert(ctParts.length == 2);
          SpecialEffects(ctParts(0), ctParts(1), parts(2), Some(parts(3)))
        } else {
          throw new IllegalArgumentException(s"Could not extract effects parts from ${s}")
        }
      }
      case _ if s.startsWith("@") => ??? // can not extract the field again
    }
  }

  private def extractTarget(prefix: String, s: String): String = {
    val target = s.substring(prefix.length()).trim().replaceAll(""""""", "");
    if (!target.isEmpty()) {
      target
    } else {
      throw new IllegalArgumentException(s"Could not extract target from ${s}")
    }
  }

  final case class FromField(f: FieldLike[ChatCommand]) extends ChatCommand {
    val fa = AutocalcExprs.FieldAccess(f, false); // never label ChatCommand fields

    override def render: String = s"${fa.render} ";
    def apiType: Option[ChatType.ChatType] = None;
  }

  case object Default extends ChatCommand {
    override def render: String = " ";
    def apiType: Option[ChatType.ChatType] = Some(ChatType.general);
  }

  final case class API(command: String, args: String) extends ChatCommand {
    override def render: String = s"!$command $args";
    def apiType: Option[ChatType.ChatType] = Some(ChatType.api);
  }

  case object GM extends ChatCommand {
    override def render: String = "/w gm ";
    def apiType: Option[ChatType.ChatType] = Some(ChatType.whisper);
  }

  final case class Whisper(target: String) extends ChatCommand {
    override def render: String = s"""/w "${target}" """;
    def apiType: Option[ChatType.ChatType] = Some(ChatType.whisper);
  }

  case object Emote extends ChatCommand {
    override def render: String = "/em ";
    def apiType: Option[ChatType.ChatType] = Some(ChatType.emote);
  }

  case object OutOfCharacter extends ChatCommand {
    override def render: String = "/ooc ";
    def apiType: Option[ChatType.ChatType] = Some(ChatType.general);
  }

  final case class SpecialEffects(`type`: String, colour: String, sourceId: String, targetId: Option[String] = None) extends ChatCommand {
    override def render: String = targetId match {
      case Some(tid) => "/fx ${type}-${colour} ${sourceId} ${tid}";
      case None      => "/fx ${type}-${colour} ${sourceId}";
    }
    def apiType: Option[ChatType.ChatType] = None;
  }

  case object Description extends ChatCommand {
    override def render: String = "/desc ";
    def apiType: Option[ChatType.ChatType] = Some(ChatType.desc);
  }

  final case class SpeakAs(target: String) extends ChatCommand {
    override def render: String = s"""/as "${target}" """;
    def apiType: Option[ChatType.ChatType] = Some(ChatType.general);
  }

  final case class EmoteAs(target: String) extends ChatCommand {
    override def render: String = s"""/emas "${target}" """;
    def apiType: Option[ChatType.ChatType] = Some(ChatType.emote);
  }
}

object FXType {
  val beam = "beam";
  val bomb = "bomb";
  val breath = "breath";
  val bubbling = "bubbling";
  val burn = "burn";
  val burst = "burst";
  val explode = "explode";
  val glow = "glow";
  val missile = "missile";
  val nova = "nova";
  val splatter = "splatter";
}

object FXColour {
  val acid = "acid";
  val blood = "blood";
  val charm = "charm";
  val death = "death";
  val fire = "fire";
  val frost = "frost";
  val holy = "holy";
  val magic = "magic";
  val slime = "slime";
  val smoke = "smoke";
  val water = "water";
}
