# Security Policy

## Reporting a Vulnerability

Do **not** open a public issue for security vulnerabilities. Report privately:

**Email:** adnanraza3435@gmail.com

Include: version, R version, OS, a description of the issue and impact, a
minimal reproduction, and a suggested fix if known. The maintainer will
acknowledge and work toward coordinated disclosure. Do not disclose publicly
until a fix is available unless otherwise agreed.

## Scope

gcvisualyst parses DNA sequences (FASTA/FASTQ) and user-supplied data. Input
handling, the FASTA parser, and anything processing untrusted data are the
primary security surface.

## Supported Versions

| Version | Supported |
| ------- | --------- |
| latest release | ✅ |
| older releases | ❌ — upgrade recommended |
