import { Page, Locator } from '@playwright/test';

export class ScrumPage {
    readonly page: Page;

    constructor(page: Page) {
        this.page = page;
    }

    async goto() {
        await this.page.goto('/SCRUM.html', { timeout: 60000 });
    }

    async accessScrumView() {
        await this.page.getByRole('link', { name: 'Scrum View' }).click();
        await this.page.waitForURL(/SCRUM\.html/);
    }

    async clickTask(taskName: string) {
        // Find all TODO spans and find the one followed by the taskName.
        // This is more robust than XPath for special characters.
        const todoSpans = await this.page.locator('span.org-todo').all();
        for (const span of todoSpans) {
            const nextText = await span.evaluate(node => node.nextSibling?.textContent || '');
            if (nextText.includes(taskName)) {
                await span.click();
                await this.page.waitForURL(/ORG-TASK\.html/);
                return;
            }
        }
        throw new Error(`Could not find task "${taskName}"`);
    }
}
