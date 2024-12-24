#include <furi.h>
#include <cli/cli.h>
#include <toolbox/pipe.h>

#include "test_runner.h"

void unit_tests_cli(PipeSide* pipe, FuriString* args, void* context) {
    UNUSED(context);

    TestRunner* test_runner = test_runner_alloc(pipe, args);
    test_runner_run(test_runner);
    test_runner_free(test_runner);
}

void unit_tests_on_system_start(void) {
#ifdef SRV_CLI
    Cli* cli = furi_record_open(RECORD_CLI);
    cli_add_command(cli, "unit_tests", CliCommandFlagDefault, unit_tests_cli, NULL);
    furi_record_close(RECORD_CLI);
#endif
}
