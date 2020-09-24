package main

import (
	"github.com/pulumi/pulumi/sdk/v2/go/pulumi"
)

func main() {
	pulumi.Run(func(ctx *pulumi.Context) error {

		geoffreyhuntley_com(ctx)
		ghuntley_com(ctx)
		ghuntley_dev(ctx)
		ghuntley_net(ctx)

		return nil
	})
}
