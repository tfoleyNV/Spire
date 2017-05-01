//SPIRE_TEST_OPTS:-no-checking -target dxbc-assembly -profile ps_4_0 -entry main

// We need to confirm that when there is an error in
// the input code, we allow the downstream compiler
// to detect and report the error, not us...

// This file presents a simple case, where we forgot a semicolon.

float4 main() : SV_Target
{
	return 0.0
}