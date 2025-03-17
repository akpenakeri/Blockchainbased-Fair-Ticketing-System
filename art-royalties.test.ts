import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const artist = accounts.get("wallet_1")!;
const subscriber = accounts.get("wallet_2")!;

describe("art-royalties", () => {
  it("allows artists to register artwork and users to subscribe", () => {
    // First register the artwork
    const registerCall = simnet.callPublicFn(
      "art-royalties",
      "register-artwork",
      [Cl.stringAscii("My Awesome Art")],
      artist
    );
    expect(registerCall.result).toBeOk(Cl.bool(true));

    // Then subscribe to the artist
    const subscribeCall = simnet.callPublicFn(
      "art-royalties",
      "subscribe-to-artist",
      [Cl.principal(artist)],
      subscriber
    );
    expect(subscribeCall.result).toBeOk(Cl.bool(true));
  });

  it("correctly checks subscription status", () => {
    const checkSubCall = simnet.callReadOnlyFn(
      "art-royalties",
      "check-subscription",
      [Cl.principal(subscriber), Cl.principal(artist)],
      subscriber
    );
    expect(checkSubCall.result).toEqual(Cl.bool(false));
  });

  it("retrieves artwork details", () => {
    // First register the artwork to ensure it exists
    const registerCall = simnet.callPublicFn(
      "art-royalties",
      "register-artwork",
      [Cl.stringAscii("My Awesome Art")],
      artist
    );
    expect(registerCall.result).toBeOk(Cl.bool(true));

    // Then retrieve and check the artwork details
    const getArtworkCall = simnet.callReadOnlyFn(
      "art-royalties",
      "get-artwork",
      [Cl.principal(artist)],
      artist
    );
    expect(getArtworkCall.result).toBeSome(
      Cl.tuple({
        title: Cl.stringAscii("My Awesome Art"),
        artist: Cl.principal(artist),
        "subscription-price": Cl.uint(10000000),
        active: Cl.bool(true),
      })
    );
  });
});
