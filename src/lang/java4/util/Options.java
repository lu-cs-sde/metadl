/* Copyright (c) 2005-2008, Torbjorn Ekman
 *                    2018, Jesper Öqvist
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.extendj.ast;

import java.io.FileReader;
import java.io.StreamTokenizer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * Tracks command-line arguments.
 *
 * <p>Tracked options are added by the add*Option methods.
 * Command-line arguments are parsed by the addOptions method.
 *
 * <p>The list of filename arguments to ExtendJ is accessed via the files() method.
 */
public class Options {
  static class Option {
    public String name;

    public boolean hasValue;

    public boolean isCollection;

    public Option(String name, boolean hasValue, boolean isCollection) {
      this.name = name;
      this.hasValue = hasValue;
      this.isCollection = isCollection;
    }
  }

  private final Map<String,Object> options = new HashMap<String,Object>();

  private final Map<String,Option> optionDescriptions = new HashMap<String,Option>();

  private final Set<String> files = new LinkedHashSet<String>();

  public Collection<String> files() {
    return files;
  }

  public void initOptions() {
    options.clear();
    optionDescriptions.clear();
    files.clear();
  }

  public void addKeyOption(String name) {
    if (optionDescriptions.containsKey(name)) {
      throw new Error("Command line definition error: option description for "
          + name + " is multiply declared");
    }
    optionDescriptions.put(name, new Option(name, false, false));
  }

  public void addKeyValueOption(String name) {
    if (optionDescriptions.containsKey(name)) {
      throw new Error("Command line definition error: option description for "
          + name + " is multiply declared");
    }
    optionDescriptions.put(name, new Option(name, true, false));
  }

  public void addKeyCollectionOption(String name) {
    if (optionDescriptions.containsKey(name)) {
      throw new Error("Command line definition error: option description for "
          + name + " is multiply declared");
    }
    optionDescriptions.put(name, new Option(name, true, true));
  }

  public void addOptionDescription(String name, boolean value) {
    if (optionDescriptions.containsKey(name)) {
      throw new Error("Command line definition error: option description for "
          + name + " is multiply declared");
    }
    optionDescriptions.put(name, new Option(name, value, false));
  }

  public void addOptionDescription(String name, boolean value, boolean isCollection) {
    if (optionDescriptions.containsKey(name)) {
      throw new Error("Command line definition error: option description for "
          + name + " is multiply declared");
    }
    optionDescriptions.put(name, new Option(name, value, isCollection));
  }

  /**
   * Parse options from command-line arguments.
   * Options starting with the at-sign are treated as argument files.
   * The argument file is expanded in place before parsing other arguments.
   *
   * @param args command-line arguments
   */
  public void addOptions(String[] args) {
    java.util.List<String> argList = new ArrayList<String>();

    // Expand argument files.
    for (int i = 0; i < args.length; i++) {
      String arg = args[i];
      if (arg.length() > 1 && arg.startsWith("@")) {
        if (arg.startsWith("@@")) {
          // Escape the double at.
          argList.add(arg.substring(1));
        } else {
          String fileName = arg.substring(1);
          try {
            StreamTokenizer tokenizer = new StreamTokenizer(new FileReader(fileName));
            tokenizer.resetSyntax();
            tokenizer.whitespaceChars(' ',' ');
            tokenizer.whitespaceChars('\t','\t');
            tokenizer.whitespaceChars('\f','\f');
            tokenizer.whitespaceChars('\n','\n');
            tokenizer.whitespaceChars('\r','\r');
            tokenizer.wordChars(33,255);
            tokenizer.commentChar('#');
            tokenizer.quoteChar('"');
            tokenizer.quoteChar('\'');
            while (tokenizer.nextToken() != tokenizer.TT_EOF) {
              argList.add(tokenizer.sval);
            }
          } catch (java.io.FileNotFoundException e) {
            System.err.println("Argument file not found: " + fileName);
          } catch (java.io.IOException e) {
            System.err.println("Exception: " + e.getMessage());
          }
        }
      } else {
        argList.add(arg);
      }
    }

    Iterator<String> all = argList.iterator();
    while (all.hasNext()) {
      String arg = all.next();
      if (arg.startsWith("-")) {
        if (!optionDescriptions.containsKey(arg)) {
          if (arg.startsWith("-X")) {
            continue;
          }
          throw new Error("Command line argument error: option " + arg + " is not defined");
        }
        Option o = (Option) optionDescriptions.get(arg);

        if (!o.isCollection && options.containsKey(arg)) {
          throw new Error("Command line argument error: option " + arg + " is multiply defined");
        }

        if (o.hasValue) {
          String value = null;
          if (!all.hasNext()) {
            throw new Error("Command line argument error: value missing for key " + arg);
          }
          value = all.next();
          if (value.startsWith("-")) {
            throw new Error("Command line argument error: expected value for key " + arg
                + ", but found option " + value);
          }

          if (o.isCollection) {
            Collection<String> c = (Collection<String>) options.get(arg);
            if (c == null) {
              c = new ArrayList<String>();
            }
            c.add(value);
            options.put(arg, c);
          } else {
            options.put(arg, value);
          }
        } else {
          options.put(arg, null);
        }
      } else {
        files.add(arg);
      }
    }
  }

  public boolean hasOption(String name) {
    return options.containsKey(name);
  }

  public void setOption(String name) {
    options.put(name, null);
  }

  public boolean hasValueForOption(String name) {
    return options.containsKey(name) && options.get(name) != null;
  }

  public String getValueForOption(String name) {
    if (!hasValueForOption(name)) {
      throw new Error("Command line argument error: key " + name + " does not have a value");
    }
    return (String) options.get(name);
  }

  public void setValueForOption(String value, String option) {
    options.put(option, value);
  }

  public Collection<String> getValueCollectionForOption(String name) {
    if (!hasValueForOption(name)) {
      throw new Error("Command line argument error: key " + name + " does not have a value");
    }
    return (Collection<String>) options.get(name);
  }

  public boolean verbose() {
    return hasOption("-verbose");
  }
}
