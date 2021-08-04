package lang;

import java.io.IOException;
import java.io.File;;
import java.sql.SQLException;

import eval.EvaluationContext;
import incremental.ProgramSplit;
import lang.ast.Program;

import static prof.Profile.profile;

public class CompilerInput {
	private CmdLineOpts opts;
	private Program program = null;
	private boolean checked = false;
	private boolean edb_import_evaluated = false;
	private ProgramSplit program_split = null;
	private String timer_parent = "main";

	public CompilerInput(CmdLineOpts opts) {
		this.opts = opts;
	}

	public void
	setTimerParent(String timer_parent) {
		this.timer_parent = timer_parent;
	}

	public CmdLineOpts
	getOpts() {
		return this.opts;
	}

	public File
	getSourceFile() {
		return new File(opts.getInputFile());
	}

	public Program
	getUncheckedProgram() {
		if (this.program == null) {
			try {
				this.program = Compiler.parseProgram(this.timer_parent, this.opts);
			} catch (beaver.Parser.Exception | IOException exn) {
				throw new RuntimeException(exn);
			}
		}
		return this.program;
	}

	public Program
	getProgram() {
		// ensure that the program is loaded
		this.getUncheckedProgram();
		this.checkProgram();
		return this.program;
	}

	public void
	checkProgram() {
		if (!this.checked) {
			Compiler.checkProgram(this.getUncheckedProgram(), this.opts);
			this.checked = true;
		}
	}

	public EvaluationContext
	getCtx() {
		return this.getUncheckedProgram().evalCtx();
	}

	public ProgramSplit
	getProgramSplit() {
		if (this.program_split == null) {
			Program prog = getProgram();
			evalIDBandIMPORT();
			profile().startTimer(timer_parent, "local_and_global_split");
			// split program into local and global parts
			ProgramSplit split = new ProgramSplit(prog);
			if (!split.canEvaluateIncrementally()) {
				throw new RuntimeException("Cannot evaluate this program incrementally.");
			}
			profile().stopTimer(timer_parent, "local_and_global_split");
			this.program_split = split;
		}
		return this.program_split;
	}

	/**
	 * Checks whether the specified files is either missing or older than the input file
	 */
	public boolean
	isOutOfDate(File file) {
		return (!file.exists()
			|| getSourceFile().lastModified() > file.lastModified());
	}

	/**
	 * Evaluate EDB and import statements, usually for hybrid evaluation
	 */
	public void
	evalIDBandIMPORT() {
		if (!this.edb_import_evaluated) {
			try {
				this.getProgram().evalEDB(this.getCtx(), this.opts);
				this.getProgram().evalIMPORT(this.getCtx(), this.opts);
			} catch (SQLException |	IOException exn) {
				throw new RuntimeException(exn);
			}
			this.edb_import_evaluated = true;
		}
	}
}
