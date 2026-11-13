// Vérifie le mot de passe de l'intranet côté serveur, puis redirige vers mnesis.ca.
// Le mot de passe n'est pas dans le dépôt : il se règle dans Netlify, variable INTRANET_MOT_DE_PASSE.

const DESTINATION = "https://mnesis.ca";

async function empreinte(texte: string): Promise<string> {
  const octets = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(texte));
  return Array.from(new Uint8Array(octets), (o) => o.toString(16).padStart(2, "0")).join("");
}

export default async (requete: Request) => {
  const url = new URL(requete.url);
  if (requete.method !== "POST") return Response.redirect(new URL("/intranet.html", url), 303);

  const formulaire = await requete.formData();
  const en = formulaire.get("langue") === "en";
  const attendu = Netlify.env.get("INTRANET_MOT_DE_PASSE") ?? "";
  const saisi = String(formulaire.get("mot-de-passe") ?? "");

  // Comparer les empreintes évite de révéler la longueur ou le début du mot de passe
  if (attendu !== "" && (await empreinte(saisi)) === (await empreinte(attendu))) {
    return Response.redirect(DESTINATION, 303);
  }
  return Response.redirect(new URL(en ? "/en/intranet.html?erreur=1" : "/intranet.html?erreur=1", url), 303);
};

export const config = { path: "/intranet-acces" };
